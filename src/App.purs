module App (component) where

import Prelude hiding (div)

import Control.Monad.RWS (modify_)
import Effect.Aff (Aff)
import Halogen (Component, HalogenM, Slot, ComponentHTML, defaultEval, mkComponent, mkEval) as H
import Halogen.HTML (HTML, a, div, h1, nav, slot_, text)
import Halogen.HTML.Events (onClick)
import Checklists (component) as Checklists
import Notes (component) as Notes
import Type.Prelude (Proxy(..))
import Utils (class_)

type OpaqueSlot slot = forall query. H.Slot query Void slot
type ChildSlots = ( notes :: OpaqueSlot Unit
                  , checklists :: OpaqueSlot Unit
                  )
data Action = SwitchComponent
data State = CurrentlyDisplayingNotes
           | CurrentlyDisplayingChecklists

derive instance stateEqInstance :: Eq State

component :: forall q i. H.Component q i Void Aff
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

initialState :: forall i. i -> State
initialState = const CurrentlyDisplayingNotes

handleAction :: Action -> H.HalogenM State Action ChildSlots Void Aff Unit
handleAction SwitchComponent = do
  modify_ (case _ of
           CurrentlyDisplayingNotes -> CurrentlyDisplayingChecklists
           CurrentlyDisplayingChecklists -> CurrentlyDisplayingNotes)

render :: State -> H.ComponentHTML Action ChildSlots Aff
render state =
  div [ class_ "container" ]
    [ h1 [ class_ "text-center" ] [ text "FAVS" ]
    , nav [ class_ "row nav nav-tabs" ] [ tab "Notes" (state == CurrentlyDisplayingNotes), tab "Checklists" (state == CurrentlyDisplayingChecklists)]
    , currentComponent state
    , div [ class_ "bottom-space" ] []
    ]

currentComponent :: State -> H.ComponentHTML Action ChildSlots Aff
currentComponent CurrentlyDisplayingNotes = slot_ (Proxy :: _ "notes") unit Notes.component unit
currentComponent CurrentlyDisplayingChecklists = slot_ (Proxy :: _ "checklists") unit Checklists.component unit

tab :: forall w. String -> Boolean -> HTML w Action
tab title active =
  div [ class_ "col text-center nav-item px-0" ]
    [ a [ class_ $ "nav-link" <> (if active then " active" else ""), onClick (const SwitchComponent)] [ text title ] ]

