module App (component, Note(..), NoteContent, StorageId) where

import Prelude hiding (div)

import Affjax (Error, printError)
import Affjax.RequestBody (RequestBody(..))
import Affjax.ResponseFormat (json)
import Affjax.Web (Response, get, post, put)
import Control.Monad.RWS (gets, modify_)
import Data.Argonaut.Core (Json, jsonEmptyObject)
import Data.Argonaut.Decode (class DecodeJson, JsonDecodeError, decodeJson, (.:))
import Data.Argonaut.Encode (class EncodeJson, encodeJson, (:=), (~>))
import Data.Array (index, length, mapWithIndex, null, snoc)
import Data.Either (Either, either)
import Data.Generic.Rep (class Generic)
import Data.Lens (Lens', lens, lens', (.~), (^.))
import Data.Maybe (Maybe, maybe)
import Data.Newtype (unwrap, wrap)
import Data.Show.Generic (genericShow)
import Data.String (Pattern(Pattern), split) as Str
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log, logShow)
import Foreign.Object (Object)
import Halogen (Component, ComponentHTML, HalogenM, modify_, mkComponent, mkEval, defaultEval) as H
import Halogen.HTML (HTML, button, div, h1, h2, input, li, nav, section, span, text, textarea)
import Halogen.HTML.Events (onBlur, onClick, onValueChange)
import Halogen.HTML.Properties (value)
import Halogen.HTML.Properties as Properties

type State = { notes :: Array Note
             , editingState :: EditingState
             }

data EditingState = None
                  | EditingNoteTitle Int
                  | EditingNoteContent Int

data Note = NewNote { content   :: NoteContent }
          | ServerNote { content   :: NoteContent
                       , storageId :: StorageId }

derive instance noteGenericInstance :: Generic Note _
instance noteShowInstance :: Show Note where
  show = genericShow

_content :: Lens' Note NoteContent
_content = lens' $ (\note -> Tuple (getContent note) (setContent note))


getContent :: Note -> NoteContent
getContent (NewNote n) = n.content
getContent (ServerNote n) = n.content

setContent :: Note -> NoteContent -> Note
setContent (NewNote _) newContent = NewNote { content: newContent }
setContent (ServerNote n) newContent = ServerNote (n { content = newContent })

_title :: Lens' Note String
_title = _content <<< (lens _.title $ _ { title = _ })

_noteContent :: Lens' Note String
_noteContent = _content <<< (lens _.noteContent $ _ { noteContent = _ })

instance noteDecodeJsonInstance :: DecodeJson Note where
  decodeJson :: Json -> Either JsonDecodeError Note
  decodeJson json = do
    dec <- decodeJson json
    content <- dec .: "content"
    title <- content .: "title"
    noteContent <- content .: "noteContent"
    either (const $ pure $ NewNote { content: { title: title, noteContent: noteContent } })
           (decodeServerNote title noteContent)
           (dec .: "storageId")
    where
      decodeServerNote :: String -> String -> Object Json -> Either JsonDecodeError Note
      decodeServerNote title noteContent storageIdObj = do
        version <- storageIdObj .: "version"
        id <- storageIdObj .: "id"
        pure $ ServerNote { content: { title: title, noteContent: noteContent }
                          , storageId: { version: version, id: id }}

instance noteEncodeJson :: EncodeJson Note where
  encodeJson :: Note -> Json
  encodeJson (NewNote { content: { title, noteContent } }) =
    encodeContentObj title noteContent
  encodeJson (ServerNote { content: { title, noteContent }, storageId: { version, id }}) =
    cont ~> storage ~> jsonEmptyObject
    where
      cont :: Tuple String Json
      cont = "content" := encodeContentObj title noteContent
      storage :: Tuple String Json
      storage = "storageId" := encodeStorageIdObj id version

encodeContentObj :: String -> String -> Json
encodeContentObj title noteContent =
  "title" := title
    ~> "noteContent" := noteContent
    ~> jsonEmptyObject

encodeStorageIdObj :: String -> String -> Json
encodeStorageIdObj id version =
  "id" := id
    ~> "version" := version
    ~> jsonEmptyObject

type NoteId = Maybe { version :: String, id :: String }
             -- ^ The Nothing value represents the id of the NewNote
type NoteContent = { noteContent :: String, title :: String }

type StorageId = { version :: String, id :: String }

data Action = CreateNewNote
            | EditNoteTitle Int
            | NoteTitleChanged Int Note String
            | NoteContentChanged Int Note String
            | EditDone
            | EditNoteContent Int

component :: forall q o. H.Component q (Array Note) o Aff
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

initialState :: Array Note -> State
initialState notes = { notes: notes, editingState: None }

newNote :: Note
newNote = NewNote { content: { title: "What's your new title?", noteContent: "What's your new content?" } }

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  div [] 
    [ section [classes "top-bar"] $
        [ h1 [] [ text "FAVS" ] 
        , nav [ classes "tabs" ] [ tab ]
        ]
    , section [ classes "notes" ]
    (notesRender state.editingState (snoc state.notes newNote))
    , nav [ classes "bottom-bar" ]
        [ button [ classes "btn", onClick (\_ -> CreateNewNote) ] [ text "+" ] ]
    ]

tab :: forall w i. HTML w i
tab =
  div [ classes "tab" ]
      [ span [ classes "tab-link active" ] [ text "Notes" ] ]

notesRender :: forall w. EditingState -> Array Note -> Array (HTML w Action)
notesRender editingState notes = (if (null notes) then noNotesDiv else (mapWithIndex (noteRender editingState) notes))

noNotesDiv :: forall w i. Array (HTML w i)
noNotesDiv = [ div [] [ text "There are no notes to display" ] ]

noteRender :: forall w. EditingState -> Int -> Note -> HTML w Action
noteRender editingState idx note =
  li [ classes "note" ]
    [ noteTitleRender editingState idx note
    , noteContentRender editingState idx note
    ]

noteContentRender :: forall w. EditingState -> Int -> Note -> HTML w Action
noteContentRender (EditingNoteContent editIdx) idx note
  | editIdx == idx = textarea [ onBlur (const EditDone), onValueChange  $ NoteContentChanged idx note, value (note ^. _noteContent) ]
  | otherwise = div [ onClick (const $ EditNoteContent idx) ] [ text (note ^. _noteContent) ]
noteContentRender _ idx note = div [ onClick (const $ EditNoteContent idx) ] [ text (note ^. _noteContent) ]

noteTitleRender :: forall w. EditingState -> Int -> Note -> HTML w Action
noteTitleRender (EditingNoteTitle editIdx) idx note
  | editIdx == idx = input [ onBlur (const EditDone), onValueChange  $ NoteTitleChanged idx note, value (note ^. _title)]
  | otherwise = h2  [ onClick (const $ EditNoteTitle idx) ] [ text (note ^. _title)]
noteTitleRender _ idx note = h2  [ onClick (const $ EditNoteTitle idx) ] [ text (note ^. _title)]

handleAction :: forall o. Action -> H.HalogenM State Action () o Aff Unit
handleAction = case _ of
  CreateNewNote -> 
    H.modify_ \st -> st { notes = snoc st.notes (NewNote { content: { title: "What's your new content?", noteContent: "" } }) 
                        , editingState = EditingNoteTitle (length st.notes)}
  EditNoteTitle idx ->
    modify_ \st -> st { editingState = EditingNoteTitle idx }
  EditNoteContent idx ->
    modify_ \st -> st { editingState = EditingNoteContent idx }
  NoteTitleChanged idx note newTitle -> updateNoteWithSaveAndRefreshNotes _title idx note newTitle
  NoteContentChanged idx note newContent -> updateNoteWithSaveAndRefreshNotes _noteContent idx note newContent
  EditDone -> do 
    liftEffect $ log "Edit Done"
    modify_ \st -> st { editingState = None }


updateNoteWithSaveAndRefreshNotes :: forall a o. Lens' Note a -> Int -> Note -> a -> H.HalogenM State Action () o Aff Unit
updateNoteWithSaveAndRefreshNotes lens_ idx note newVal = do
  oldNotes <- gets _.notes
  liftEffect $ log "old notes gotten"
  let
    modifiedNote = index (snoc oldNotes newNote) idx >>= ((lens_ .~ newVal) >>> pure)
  liftEffect $ log $ "New note: " <> show modifiedNote
  maybe (log $ "Unable to modify note at index " <> show idx <> ": note cannot be found in model")
        (case note of
          NewNote _ -> postNote
          ServerNote _ -> putNote)
        modifiedNote

checkStatusAndGetNotes :: forall o. Response Json -> H.HalogenM State Action () o Aff Unit
checkStatusAndGetNotes resp = do
  if unwrap resp.status >= 300 || unwrap resp.status < 200
    then liftEffect $ log $ "Wrong status response for post note: " <> show resp.status
    else do
      notesResp <- liftAff $ get json "/api/note"
      either handleError
             (\notesjson -> do
               either (liftEffect <<< logShow)
                      (\newNotes -> modify_ \st -> st { notes = newNotes })
                      notesjson)
             (decodeJson <$> _.body <$> notesResp)                                                 

postNote :: forall o. Note -> H.HalogenM State Action () o Aff Unit
postNote note = do
    resp <- liftAff $ post json "/api/note" ((pure <<< Json <<< encodeJson) note)
    either handleError
           checkStatusAndGetNotes
           resp -- /!\ errors in call are not handled here. We should handle all non 2XX responses
    modify_ \st -> st { editingState = None }

putNote :: forall o. Note -> H.HalogenM State Action () o Aff Unit
putNote note = do
    resp <- liftAff $ put json "/api/note" ((pure <<< Json <<< encodeJson) note)
    either handleError
           checkStatusAndGetNotes
           resp -- /!\ errors in call are not handled here. We should handle all non 2XX responses
    modify_ \st -> st { editingState = None }

handleError :: forall o. Error -> H.HalogenM State Action () o Aff Unit
handleError = liftAff <<< log <<< printError

classes :: forall r i. String -> Properties.IProp (class :: String | r) i
classes = Str.split (Str.Pattern " ") >>> (map wrap) >>> Properties.classes

type FatalError = String
