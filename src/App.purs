module App where

import Prelude hiding (div)

import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Data.Array (snoc, null)
import Data.Either (either, note)
import Data.Maybe (Maybe(Nothing), maybe)
import Data.Newtype (wrap)
import Data.String (Pattern(Pattern), null, split) as Str
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Halogen (Component, ComponentHTML, HalogenM, modify_, mkComponent, mkEval, defaultEval) as H
import Halogen.HTML (HTML, attr, button, div, h1, h2, li, nav, section, span, text)
import Halogen.HTML.Events (onClick, onFocusOut)
import Halogen.HTML.Properties (IProp)
import Halogen.HTML.Properties as Properties
import Web.DOM.Element (fromEventTarget, toNode)
import Web.DOM.Node (childNodes, nodeValue)
import Web.DOM.NodeList (item)
import Web.Event.Event (target)
import Web.UIEvent.FocusEvent (FocusEvent, toEvent)

type State = { notes        :: Array Note }

type Note = { content :: NoteContent
            , storageId :: NoteId }

type NoteId = Maybe { version :: String, id :: String }
             -- ^ The Nothing value represents the id of the NewNote
type NoteContent = { noteContent :: String, title :: String }

data Action = NewNote
            | SaveNote FocusEvent-- NoteId

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
     [ h2  [ saveOnBlur Nothing, contenteditable ] [ text "What's your new title?"   ]
     , div [ contenteditable, saveOnBlur Nothing ] [ text "What's your new content?" ]
     ]

handleAction :: forall o. Action -> H.HalogenM State Action () o Aff Unit
handleAction = case _ of
  NewNote -> 
    H.modify_ \st -> st { notes = snoc st.notes { storageId: Nothing
                                                , content: { title: "What's your new content?", noteContent: "" } } }
  SaveNote ev -> liftEffect $ do
    res <- runExceptT changedString
    either log log res
    where
      changedString ::  ExceptT FatalError Effect String
      changedString = do
        tg <- (hoistToExceptT "no target" <<< target <<< toEvent) ev
        node <- toNode <$> (hoistToExceptT "no element for target" $ fromEventTarget tg)
        childNode <- wrap $ childNodes node >>= item 0 >>= (note "no child node" >>> pure)
        wrap $ note "no value in child node" <$> nodeValue childNode

contenteditable :: forall r i. IProp r i
contenteditable = attr (wrap "contenteditable") ""

saveOnBlur :: forall r. NoteId -> IProp (onFocusOut :: FocusEvent | r) Action
saveOnBlur id = onFocusOut $ (\ev -> SaveNote ev)

classes :: forall r i. String -> Properties.IProp (class :: String | r) i
classes = Str.split (Str.Pattern " ") >>> (map wrap) >>> Properties.classes

maybeStr :: String -> String -> String
maybeStr default s = if (Str.null s) then default else s

hoistToExceptT :: forall e m a. Monad m => e -> Maybe a -> ExceptT e m a
hoistToExceptT e = maybe (throwError e) pure

type FatalError = String
