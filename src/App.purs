module App (component, Note(..), NoteContent, StorageId) where

import Prelude hiding (div)

import Affjax (Error, printError)
import Affjax.RequestBody (RequestBody(..))
import Affjax.ResponseFormat (json)
import Affjax.Web (Response, delete, post, put)
import Affjax.Web (get) as Affjax
import Control.Alt ((<|>))
import Control.Monad.Error.Class (catchError, throwError)
import Control.Monad.Except (ExceptT(..), runExceptT, withExceptT)
import Control.Monad.RWS (get, gets, modify_)
import Control.Monad.Trans.Class (lift)
import Data.Argonaut.Core (Json, jsonEmptyObject)
import Data.Argonaut.Decode (class DecodeJson, JsonDecodeError, decodeJson, (.:))
import Data.Argonaut.Encode (class EncodeJson, encodeJson, (:=), (~>))
import Data.Array (index, length, mapWithIndex, null, snoc)
import Data.Bifunctor (lmap)
import Data.Either (Either, either)
import Data.Generic.Rep (class Generic)
import Data.Int (floor, toNumber)
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
import Halogen (Component, ComponentHTML, HalogenM, defaultEval, getRef, mkComponent, mkEval, put) as H
import Halogen.HTML (HTML, button, div, h1, h2, header, i, input, li, nav, section, span, text, textarea, ul)
import Halogen.HTML.Events (onBlur, onClick, onValueChange)
import Halogen.HTML.Properties (ref, value)
import Halogen.HTML.Properties as Properties
import Web.DOM (Element)
import Web.DOM.Element (getBoundingClientRect, getElementsByClassName)
import Web.DOM.HTMLCollection (item)
import Web.HTML (window)
import Web.HTML.HTMLElement (focus, fromElement) as HTMLElement
import Web.HTML.HTMLInputElement (fromElement) as InputElement
import Web.HTML.HTMLInputElement (select)
import Web.HTML.Window (innerHeight, scroll)

type Output = Void
type NoteAppM = H.HalogenM State Action () Output Aff
type ErrorNoteAppM = ExceptT FatalError NoteAppM
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
            | NoteTitleChanged Int String
            | NoteContentChanged Int String
            | EditDone
            | EditNoteContent Int
            | DeleteNote StorageId

component :: forall q. H.Component q (Array Note) Output Aff
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

-- ==================================== RENDERING ===========================================

render :: forall m. State -> H.ComponentHTML Action () m
render { notes, editingState } =
  div [ classes "container" ]
    [ h1 [ classes "row justify-content-center" ] [ text "FAVS" ]
    , nav [ classes "row nav nav-tabs nav-fill" ] [ tab ]
    , if (null notes) then noNotesDiv else ul [ classes "list-group" ] (mapWithIndex (noteRender editingState) notes)
    , section [ classes "row" ]
        [ button [ classes "btn btn-primary", onClick (\_ -> CreateNewNote) ] [ text "+" ] ]
    , div [ classes "bottom-space" ] []
    ]

tab :: forall w i. HTML w i
tab =
  div [ classes "nav-item px-0" ]
      [ span [ classes "nav-link active" ] [ text "Notes" ] ]

noNotesDiv :: forall w i. HTML w i
noNotesDiv = div [ classes "row" ] [ text "There are no notes to display" ]

noteRender :: forall w. EditingState -> Int -> Note -> HTML w Action
noteRender editingState idx note =
  li [ classes "row list-group-item" ] $
     [ div [ classes "col", ref (wrap $ "note-" <> show idx) ] $
       [ noteTitleRender editingState idx note
       , noteContentRender editingState idx note ]
       <> noteFooterRender note
     ]

noteFooterRender :: forall w. Note -> Array (HTML w Action)
noteFooterRender (NewNote _) = []
noteFooterRender (ServerNote { storageId }) =
  [ section [ classes "row my-2 justify-content-center" ]
    [ button [ classes "btn btn-sm btn-outline-danger", onClick (const $ DeleteNote storageId) ]
      [ i [ classes "bi bi-trash" ] [] ]  
    ]
  ]

noteContentRender :: forall w. EditingState -> Int -> Note -> HTML w Action
noteContentRender editingState idx note =
  section [ classes "row my-2" ] [ contentRender editingState ]
  where
    contentRender (EditingNoteContent editIdx)
      | editIdx == idx = textarea [ classes "form-control content-input", onBlur (const EditDone), onValueChange  $ NoteContentChanged idx, value (note ^. _noteContent) ]
    contentRender _ = div [ onClick (const $ EditNoteContent idx) ] [ text (note ^. _noteContent) ]

noteTitleRender :: forall w. EditingState -> Int -> Note -> HTML w Action
noteTitleRender editingState idx note =
  header [ classes "row my-2" ] [ contentRender editingState ]
  where
    contentRender (EditingNoteTitle editIdx)
      | editIdx == idx = input [ classes "form-control fs-2 lh-2 title-input", onBlur (const EditDone), onValueChange  $ NoteTitleChanged idx, value (note ^. _title)]
    contentRender _ = h2  [ onClick (const $ EditNoteTitle idx) ] [ text (note ^. _title) ]

classes :: forall r i. String -> Properties.IProp (class :: String | r) i
classes = Str.split (Str.Pattern " ") >>> (map wrap) >>> Properties.classes

-- ============================= Action Handling =======================================

handleAction :: Action -> NoteAppM Unit
handleAction = case _ of
  CreateNewNote -> do
    st <- get
    H.put st { notes = snoc st.notes (NewNote { content: { title: "What's your new title?", noteContent: "What's your new title?" } })
             , editingState = EditingNoteTitle (length st.notes)}
    res <- runExceptT $ goInput (length st.notes)
    either logShow pure res
  EditNoteTitle idx -> do
    modify_ \st -> st { editingState = EditingNoteTitle idx }
    res <- runExceptT $ goInput idx
    either logShow pure res
  EditNoteContent idx -> do
    modify_ \st -> st { editingState = EditingNoteContent idx }
    res <- runExceptT $ goInput idx
    either logShow pure res
  NoteTitleChanged idx newTitle -> do
    eitherRes <- runExceptT $ updateNoteWithSaveAndRefreshNotes idx _title newTitle
    either logShow pure eitherRes
  NoteContentChanged idx newContent -> do
    eitherRes <- runExceptT $ updateNoteWithSaveAndRefreshNotes idx _noteContent newContent
    either logShow pure eitherRes
  EditDone -> do
    modify_ \st -> st { editingState = None }
  DeleteNote storageId -> do
    eitherRes <- runExceptT $ do
      deleteNote storageId
      refreshNotes
    either (liftEffect <<< logShow) (const $ pure unit) eitherRes
updateNoteWithSaveAndRefreshNotes :: forall a. Int -> Lens' Note a -> a -> ErrorNoteAppM Unit
updateNoteWithSaveAndRefreshNotes idx lens_ newVal = do
  oldNotes <- gets _.notes
  let
    modifiedNote = index (snoc oldNotes newNote) idx >>= ((lens_ .~ newVal) >>> pure)
  maybe (throwError $ CustomFatalError "Unable to modify note at index ")
        writeAndRefreshThenStopEditing
        modifiedNote
  where
    writeAndRefreshThenStopEditing :: Note -> ErrorNoteAppM Unit
    writeAndRefreshThenStopEditing note = do
      resp <- writeToServer note
      if unwrap resp.status >= 300 || unwrap resp.status < 200
        then throwError $ CustomFatalError $ "Wrong status response for post note: " <> show resp.status
        else refreshNotes
      lift $ modify_ \st -> st { editingState = None }

goInput :: Int -> ErrorNoteAppM Unit
goInput idx = do
  noteElem <- getRef $ "note-" <> show idx
  scrollTo noteElem
  titleElem <- focusTitle noteElem
  maybe (throwError $ CustomFatalError $ "unable to get input element from title element") (liftEffect <<< select) $ InputElement.fromElement titleElem

scrollTo :: Element -> ErrorNoteAppM Unit
scrollTo e = do
  w <- liftEffect window
  rect <- liftEffect $ getBoundingClientRect e
  liftEffect $ log $ "client rect: " <> show rect.y
  let elemMiddle = floor $ rect.y + rect.height / toNumber 2
  vh <- liftEffect $ innerHeight w
  liftEffect $ log $ "scrolling to " <> show (elemMiddle - vh / 2)
  liftEffect $ scroll 0 (max 0 (elemMiddle - vh / 2)) w

refreshNotes :: ErrorNoteAppM Unit
refreshNotes = do
  newNotes <- getNotes
  lift $ modify_ \st -> st { notes = newNotes }

getNotes :: ErrorNoteAppM (Array Note)
getNotes = do
  jsonResponse <- withExceptT toFatalError $ ExceptT $ liftAff $ Affjax.get json "/api/note"
  (_.body >>> decodeJson >>> lmap toFatalError >>> pure >>> ExceptT) jsonResponse

deleteNote :: StorageId -> ErrorNoteAppM Unit
deleteNote { id } = do
  jsonResponse <- withExceptT toFatalError $ ExceptT $ liftAff $ delete json ("/api/note/" <> id)
  if unwrap jsonResponse.status < 200 || unwrap jsonResponse.status >= 300
    then (throwError $ CustomFatalError $ "Wrong status code when deleting note " <> id <> ": " <> show jsonResponse.status)
    else pure unit

isCreate :: Note -> Boolean
isCreate (NewNote _) = true
isCreate (ServerNote _) = false

writeToServer :: Note -> ErrorNoteAppM (Response Json)
writeToServer note = withExceptT toFatalError $ ExceptT $ liftAff $ writeFunc json "/api/note" ((pure <<< Json <<< encodeJson) note)
  where
    writeFunc = if isCreate note then post else put

-- ====================== ERRORS ==============================================
data FatalError = DecodeError JsonDecodeError
                | NetworkError Error
                | CustomFatalError String

instance fatalErrorShowInstance :: Show FatalError where
  show (DecodeError err) = "DecodeError: " <> show err
  show (NetworkError err) = "NetworkError: " <> printError err
  show (CustomFatalError err) = "CustomError: " <> err

instance fatalErrorSemigroupInstance :: Semigroup FatalError where
  append _ last = last

instance jsonDecodeErrorToFatalErrorInstance :: ToFatalError JsonDecodeError where
  toFatalError = DecodeError

instance affjaxErrorToFatalErrorInstance :: ToFatalError Error where
  toFatalError = NetworkError

class ToFatalError a where
  toFatalError :: a -> FatalError

-- ============================  Web manipulation wrapper ==================================
getRef :: String -> ErrorNoteAppM Element
getRef refStr = do
  ref <- lift $ H.getRef (wrap refStr) 
  maybe (throwError $ CustomFatalError $ "cannot get ref " <> refStr) pure ref

focusTitle :: Element -> ErrorNoteAppM Element
focusTitle noteElem = do
  title <- (getElementByClassName "title-input" noteElem) <|> (getElementByClassName "content-input" noteElem)
  focusElement title
  pure title

getElementByClassName :: String -> Element -> ErrorNoteAppM Element
getElementByClassName className element = do
  title <- liftEffect $ getElementsByClassName className element >>= item 0
  maybe (throwError $ CustomFatalError $ "unable to get note title") pure title

focusElement :: Element -> ErrorNoteAppM Unit
focusElement elem =
  maybe (throwError $ CustomFatalError $ "cannot convert element to HTML element") liftEffect $ (HTMLElement.fromElement elem >>= (pure <<< HTMLElement.focus))
