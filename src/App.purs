module App where

import Prelude hiding (div)

import Affjax.RequestBody (RequestBody(..))
import Data.Array (null, concat)
import Data.Maybe (Maybe(Nothing), maybe)
import Data.Newtype (wrap)
import Data.String (Pattern(Pattern), null, split) as Str
import Effect.Aff (Aff)
import Halogen (Component, ComponentHTML, HalogenM, modify_, mkComponent, mkEval, defaultEval) as H
import Halogen.HTML (HTML, attr, button, div, h1, h2, input, li, nav, section, span, text)
import Halogen.HTML.Events (onClick)
import Halogen.HTML.Properties (InputType(InputText), placeholder, type_)
import Halogen.HTML.Properties as Properties

type State = { notes        :: Array Note
             , newNote      :: Maybe NoteContent
             , editingState :: EditingState
             }

type Note = { content :: NoteContent
            , storageId :: { version :: String, id :: String } }

type NoteContent = { noteContent :: String, title :: String }

type EditingState = Maybe EditingNewNoteState
data EditingNewNoteState = EditingNewNoteTitle
                         | EditingNewNoteContent

editingNewNoteContent :: EditingNewNoteState -> Boolean
editingNewNoteContent EditingNewNoteTitle   = false
editingNewNoteContent EditingNewNoteContent = true

editingNewNoteTitle :: EditingNewNoteState -> Boolean
editingNewNoteTitle EditingNewNoteTitle   = true
editingNewNoteTitle EditingNewNoteContent = false

data Action = NewNote

component :: forall q o. H.Component q (Array Note) o Aff
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

initialState :: Array Note -> State
initialState notes = { notes: notes, newNote: Nothing, editingState: Nothing }

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  div [] 
    [ section [classes "top-bar"] $
      [ h1 [] [ text "FAVS" ] 
      , nav [ classes "tabs" ] [ tab ]
      ]
    , section [ classes "notes" ] (concat [notesRender state.notes, maybe [] (newNoteRender >>> pure) state.newNote])
    , nav [ classes "bottom-bar" ] [ button [ classes "btn", onClick (\_ -> NewNote) ] [ text "+" ]
    ]    ]

tab :: forall w i. HTML w i
tab =
  div [ classes "tab" ]
      [ span [ classes "tab-link active" ] [ text "Notes" ] ]

notesRender :: forall w i. Array Note -> Array (HTML w i)
notesRender notes = (if (null notes) then noNotesDiv else (map noteRender notes))

newNoteRender :: forall w i. NoteContent -> HTML w i
newNoteRender content =
  li [ classes "new-note" ]
     [ h2 [ attr (wrap "contenteditable") "" ] [ text $ maybeStr "What's your new title?" content.title ]
     , div [ attr (wrap "contenteditable") "" ] [ text $ maybeStr "What's your new content?" content.noteContent ] ]

noNotesDiv :: forall w i. Array (HTML w i)
noNotesDiv = [ div [] [ text "There are no notes to display" ] ]

noteRender :: forall w i. Note -> HTML w i
noteRender note =
  li [ classes "note" ]
    [ h2 [] [ text note.content.title ]
    , div [] [ text note.content.noteContent ]
    ]

handleAction :: forall o. Action -> H.HalogenM State Action () o Aff Unit
handleAction = case _ of
  NewNote -> 
    H.modify_ \st -> st { newNote = pure { noteContent: "", title: "" } }

classes :: forall r i. String -> Properties.IProp (class :: String | r) i
classes = Str.split (Str.Pattern " ") >>> (map wrap) >>> Properties.classes

maybeStr :: String -> String -> String
maybeStr default s = if (Str.null s) then default else s
