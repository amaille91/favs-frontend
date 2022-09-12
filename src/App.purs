module App where

import Prelude hiding (div)

import Data.Array (snoc, null)
import Data.Maybe (Maybe(Nothing))
import Data.Newtype (wrap)
import Data.String (Pattern(Pattern), null, split) as Str
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (logShow)
import Halogen (Component, ComponentHTML, HalogenM, modify_, mkComponent, mkEval, defaultEval) as H
import Halogen.HTML (HTML, attr, button, div, h1, h2, li, nav, section, span, text)
import Halogen.HTML.Events (onBlur, onClick)
import Halogen.HTML.Properties (IProp)
import Halogen.HTML.Properties as Properties
import Web.UIEvent.FocusEvent (FocusEvent)

type State = { notes        :: Array Note }

type Note = { content :: NoteContent
            , storageId :: NoteId }

type NoteId = Maybe { version :: String, id :: String }
             -- ^ The Nothing value represents the id of the NewNote
type NoteContent = { noteContent :: String, title :: String }

data Action = NewNote
            | SaveNote NoteId

component :: forall q o. H.Component q (Array Note) o Aff
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

initialState :: Array Note -> State
initialState notes = { notes: notes }

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  div [] 
    [ section [classes "top-bar"] $
        [ h1 [] [ text "FAVS" ] 
        , nav [ classes "tabs" ] [ tab ]
        ]
    , section [ classes "notes" ]
        (snoc (notesRender state.notes) newNoteRender)
    , nav [ classes "bottom-bar" ]
        [ button [ classes "btn", onClick (\_ -> NewNote) ] [ text "+" ] ]
    ]

tab :: forall w i. HTML w i
tab =
  div [ classes "tab" ]
      [ span [ classes "tab-link active" ] [ text "Notes" ] ]

notesRender :: forall w. Array Note -> Array (HTML w Action)
notesRender notes = (if (null notes) then noNotesDiv else (map noteRender notes))

noNotesDiv :: forall w i. Array (HTML w i)
noNotesDiv = [ div [] [ text "There are no notes to display" ] ]

noteRender :: forall w. Note -> HTML w Action
noteRender note =
  li [ classes "note" ]
    [ h2  [ contenteditable, saveOnBlur note.storageId ] [ text note.content.title       ]
    , div [ contenteditable, saveOnBlur note.storageId ] [ text note.content.noteContent ]
    ]

newNoteRender :: forall w. HTML w Action
newNoteRender =
  li [ classes "new-note" ]
     [ h2  [ contenteditable, saveOnBlur Nothing ] [ text "What's your new title?"   ]
     , div [ contenteditable, saveOnBlur Nothing ] [ text "What's your new content?" ]
     ]

handleAction :: forall o. Action -> H.HalogenM State Action () o Aff Unit
handleAction = case _ of
  NewNote -> 
    H.modify_ \st -> st { notes = snoc st.notes { storageId: Nothing
                                                , content: { title: "What's your new content?", noteContent: "" } } }
  SaveNote id -> liftEffect $ logShow id

contenteditable :: forall r i. IProp r i
contenteditable = attr (wrap "contenteditable") ""

saveOnBlur :: forall r. NoteId -> IProp (onBlur :: FocusEvent | r) Action
saveOnBlur id = onBlur $ const $ SaveNote id

classes :: forall r i. String -> Properties.IProp (class :: String | r) i
classes = Str.split (Str.Pattern " ") >>> (map wrap) >>> Properties.classes

maybeStr :: String -> String -> String
maybeStr default s = if (Str.null s) then default else s
