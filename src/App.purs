module App where

import Prelude

import Data.Maybe (Maybe)
import Data.List.Types (List)

import Halogen as H
import Halogen.HTML as HH
-- import Halogen.HTML.Events as HE
-- import Halogen.HTML.Properties as HP


type State = Maybe { notes :: List Note }
type Note = { name :: String }

-- instance Show Note where
--   show _ = "Note"

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
  HH.div
    []
    [ HH.text $ show state ]

handleAction :: forall o m. Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Toggle ->
    H.modify_ \st -> st
