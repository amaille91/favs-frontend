module App where

import Prelude hiding (div)

import Data.Newtype (wrap)
import Data.Array (null)

import Halogen (Component, ComponentHTML, HalogenM, modify_, mkComponent, mkEval, defaultEval) as H
import Halogen.HTML (HTML, h1, h2, div, section, nav, text)
-- import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as Properties


type State = Array Note
type Note = { content :: { noteContent :: String, title :: String }
            , storageId :: { versio :: String, id :: String } }

data Action = Toggle

component :: forall q o m. H.Component q State o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

initialState :: State -> State
initialState = identity

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  section [class_ "top-bar"] $
    [ h1 [] [ text "FAVS" ] 
    , nav [ class_ "tabs" ] [ text "Notes" ]
    , section [] (if (null state) then noNotesDiv else (map noteRender state))
    ]

noNotesDiv :: forall w i. Array (HTML w i)
noNotesDiv = [ div [] [ text "There are no notes to display" ] ]

noteRender :: forall w i. Note -> HTML w i
noteRender note =
  div [ class_ "note" ]
    [ h2 [] [ text note.content.title ]
    , div [] [ text note.content.noteContent ]
    ]

handleAction :: forall o m. Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Toggle ->
    H.modify_ \st -> st

class_ :: forall r i. String -> Properties.IProp (class :: String | r) i
class_ = Properties.class_ <<< wrap
