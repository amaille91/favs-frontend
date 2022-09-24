module App where

import Prelude hiding (div)

import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.RWS (gets, modify_)
import Data.Array (head, mapWithIndex, modifyAt, null, snoc)
import Data.Either (either, note)
import Data.Maybe (Maybe(Nothing), maybe)
import Data.Newtype (wrap)
import Data.String (Pattern(Pattern), null, split) as Str
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Halogen (Component, ComponentHTML, HalogenM, modify_, mkComponent, mkEval, defaultEval) as H
import Halogen.HTML (HTML, attr, button, div, h1, h2, input, li, nav, section, span, text, textarea)
import Halogen.HTML.Events (onBlur, onClick, onValueChange)
import Halogen.HTML.Properties (IProp, value)
import Halogen.HTML.Properties as Properties
import Web.DOM.Element (fromEventTarget, toNode)
import Web.DOM.Node (childNodes, nodeValue)
import Web.DOM.NodeList (item)
import Web.Event.Event (target)
import Web.UIEvent.FocusEvent (FocusEvent, toEvent)

type State = { notes :: Array Note
             , editingState :: EditingState
             }

data EditingState = None
                  | EditingNoteTitle Int
                  | EditingNoteContent Int

type Note = { content :: NoteContent
            , storageId :: NoteId }

type NoteId = Maybe { version :: String, id :: String }
             -- ^ The Nothing value represents the id of the NewNote
type NoteContent = { noteContent :: String, title :: String }

data Action = NewNote
            | EditNoteTitle Int
            | NoteTitleChanged Int String
            | NoteContentChanged Int String
            | EditDone
            | EditNoteContent Int
            | SaveNote FocusEvent-- NoteId

component :: forall q o. H.Component q (Array Note) o Aff
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

initialState :: Array Note -> State
initialState notes = { notes: notes, editingState: None }

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  div [] 
    [ section [classes "top-bar"] $
        [ h1 [] [ text "FAVS" ] 
        , nav [ classes "tabs" ] [ tab ]
        ]
    , section [ classes "notes" ]
        (snoc (notesRender state.editingState state.notes) newNoteRender)
    , nav [ classes "bottom-bar" ]
        [ button [ classes "btn", onClick (\_ -> NewNote) ] [ text "+" ] ]
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
  | editIdx == idx = textarea [ onBlur (const EditDone), onValueChange  $ NoteContentChanged idx, value note.content.noteContent ]
  | otherwise = div [ onClick (const $ EditNoteContent idx) ] [ text note.content.noteContent ]
noteContentRender _ idx note = div [ onClick (const $ EditNoteContent idx) ] [ text note.content.noteContent ]

noteTitleRender :: forall w. EditingState -> Int -> Note -> HTML w Action
noteTitleRender (EditingNoteTitle editIdx) idx note
  | editIdx == idx = input [ onBlur (const EditDone), onValueChange  $ NoteTitleChanged idx, value note.content.title ]
  | otherwise = h2  [ onClick (const $ EditNoteTitle idx) ] [ text note.content.title ]
noteTitleRender _ idx note = h2  [ onClick (const $ EditNoteTitle idx) ] [ text note.content.title ]


newNoteRender :: forall w. HTML w Action
newNoteRender =
  li [ classes "new-note" ]
     [ h2  [ contenteditable ] [ text "What's your new title?"   ]
     , div [ contenteditable ] [ text "What's your new content?" ]
     ]

handleAction :: forall o. Action -> H.HalogenM State Action () o Aff Unit
handleAction = case _ of
  NewNote -> 
    H.modify_ \st -> st { notes = snoc st.notes { storageId: Nothing
                                                , content: { title: "What's your new content?", noteContent: "" } } }
  EditNoteTitle idx ->
    modify_ \st -> st { editingState = EditingNoteTitle idx }
  EditNoteContent idx ->
    modify_ \st -> st { editingState = EditingNoteContent idx }
  NoteTitleChanged idx newTitle -> do
    oldNotes <- gets _.notes
    let maybeNewNotes = modifyAt idx (\n -> n { content = n.content { title = newTitle }}) oldNotes
    maybe (liftEffect $ log  $ "ERROR: unable to find note with index " <> show idx <> " while trying to update its title")
          (\newNotes -> modify_ \st -> st { notes = newNotes })
          maybeNewNotes
    modify_ \st -> st { editingState = None }
  NoteContentChanged idx newContent -> do
    oldNotes <- gets _.notes
    let maybeNewNotes = modifyAt idx (\n -> n { content = n.content { noteContent = newContent }}) oldNotes
    maybe (liftEffect $ log  $ "ERROR: unable to find note with index " <> show idx <> " while trying to update its content")
          (\newNotes -> modify_ \st -> st { notes = newNotes })
          maybeNewNotes
    modify_ \st -> st { editingState = None }
  EditDone -> 
    modify_ \st -> st { editingState = None }
  SaveNote ev -> do
    fNoteTitle <- gets (\st -> maybe "no note" (\n -> n.content.title) (head st.notes))
    liftEffect $ do
      res <- runExceptT changedString
      either log log res
      log fNoteTitle
    where
      changedString ::  ExceptT FatalError Effect String
      changedString = do
        tg <- (hoistToExceptT "no target" <<< target <<< toEvent) ev
        node <- toNode <$> (hoistToExceptT "no element for target" $ fromEventTarget tg)
        childNode <- wrap $ childNodes node >>= item 0 >>= (note "no child node" >>> pure)
        wrap $ note "no value in child node" <$> nodeValue childNode

contenteditable :: forall r i. IProp r i
contenteditable = attr (wrap "contenteditable") ""

classes :: forall r i. String -> Properties.IProp (class :: String | r) i
classes = Str.split (Str.Pattern " ") >>> (map wrap) >>> Properties.classes

maybeStr :: String -> String -> String
maybeStr default s = if (Str.null s) then default else s

hoistToExceptT :: forall e m a. Monad m => e -> Maybe a -> ExceptT e m a
hoistToExceptT e = maybe (throwError e) pure

type FatalError = String
