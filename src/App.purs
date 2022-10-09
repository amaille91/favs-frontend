module App (component) where

import Prelude hiding (div)

import Effect.Aff (Aff)
import Halogen (Component, ComponentHTML, HalogenM, Slot, defaultEval, mkComponent, mkEval) as H
import Halogen.HTML (HTML, div, h1, nav, slot_, span, text)
import Notes (component) as Notes
import Type.Prelude (Proxy(..))
import Utils (class_)

type OpaqueSlot slot = forall query. H.Slot query Void slot
type ChildSlots = (notes :: OpaqueSlot Unit)

component :: forall q i. H.Component q i Void Aff
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

initialState :: forall i. i -> Unit
initialState = const unit

handleAction :: Unit -> H.HalogenM Unit Unit ChildSlots Void Aff Unit
handleAction = const (pure unit)

render :: Unit -> H.ComponentHTML Unit ChildSlots Aff
render _ =
  div [ class_ "container" ]
    [ h1 [ class_ "text-center" ] [ text "FAVS" ]
    , nav [ class_ "nav nav-tabs nav-fill" ] [ tab ]
    , slot_ (Proxy :: _ "notes") unit Notes.component unit
    , div [ class_ "bottom-space" ] []
    ]


tab :: forall w i. HTML w i
tab =
  div [ class_ "nav-item px-0" ]
      [ span [ class_ "nav-link active" ] [ text "Notes" ] ]

