module App (component) where

import Prelude hiding (div, (/), otherwise)

import Affjax.Web (Response, post, Error)
import Affjax.RequestBody (RequestBody(..))
import Affjax.ResponseFormat (json)
import Checklists (component) as Checklists
import Control.Monad.RWS (get, modify_)
import DOM.HTML.Indexed.ButtonType (ButtonType(..))
import DOM.HTML.Indexed.InputType (InputType(..))
import Data.Argonaut.Core (Json, jsonEmptyObject)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson, (:=), (~>))
import Data.Either (Either(..), either)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (class MonadEffect)
import Effect.Console (logShow)
import Halogen (Component, HalogenM, Slot, ComponentHTML, defaultEval, mkComponent, mkEval) as H
import Halogen (HalogenM, liftEffect, subscribe)
import Halogen.HTML (HTML, a, div, h1, nav, slot_, text, form, label, input, button)
import Halogen.HTML.Events (onClick, onValueChange)
import Halogen.HTML.Properties (for, type_, name, placeholder)
import Halogen.Subscription (create, notify)
import Notes (component) as Notes
import Routing.Duplex (RouteDuplex', root, parse)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/))
import Routing.Hash (matchesWith)
import Type.Prelude (Proxy(..))
import Utils (class_)
import Web.Event.Event (Event, preventDefault)
import Web.UIEvent.MouseEvent as MouseEvent

type OpaqueSlot slot = forall query. H.Slot query Void slot
type ChildSlots = ( notes :: OpaqueSlot Unit
                  , checklists :: OpaqueSlot Unit
                  , signup :: OpaqueSlot Unit
                  )

data DefinedRoute = Note | Checklist | Signup
derive instance definedRouteGeneric :: Generic DefinedRoute _
derive instance definedRouteEq :: Eq DefinedRoute
derive instance definedRouteOrd :: Ord DefinedRoute
instance showDefinedRoute :: Show DefinedRoute where
  show = genericShow

data Route = Route DefinedRoute | NotFound
derive instance routeGeneric :: Generic Route _
derive instance routeEq :: Eq Route
derive instance ordRoute :: Ord Route
instance showRoute :: Show Route where
  show = genericShow

routeCodec :: RouteDuplex' DefinedRoute
routeCodec = root $ sum
  { "Note": "notes" / noArgs
  , "Checklist": "checklists" / noArgs
  , "Signup": "signup" / noArgs
  }

subscribeToRouting :: forall state slots output m. MonadEffect m => H.HalogenM state Action slots output m Unit
subscribeToRouting = do
  {emitter, listener} <- liftEffect create
  void $ liftEffect $ matchesWith (\s -> Right $ either (const NotFound) Route $ parse routeCodec s) \old new -> do
    when (old /= Just new) $ do
      notify listener $ RouteChanged new
  _ <- subscribe emitter
  pure unit

data Action = RouteChanged Route
            | InitializeRouting
data State = CurrentRoute Route
derive instance stateEqInstance :: Eq State

component :: forall q i. H.Component q i Void Aff
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction
                                     , initialize = pure InitializeRouting}
    }

initialState :: forall i. i -> State
initialState = const $ CurrentRoute $ Route Note

handleAction :: Action -> H.HalogenM State Action ChildSlots Void Aff Unit
handleAction (RouteChanged route) = do
  modify_ $ const $ CurrentRoute route
handleAction InitializeRouting = subscribeToRouting

render :: State -> H.ComponentHTML Action ChildSlots Aff
render (CurrentRoute (Route route)) =
  div [ class_ "container" ]
  ([ h1 [ class_ "text-center" ] [ text "FAVS" ]] <>
  (if route /= Signup then [ nav [ class_ "row nav nav-tabs" ] [ tab "Notes" (route == Note), tab "Checklists" (route == Checklist)] ] else []) <>
  [ currentComponent route
  , div [ class_ "bottom-space" ] []
  ])
render (CurrentRoute NotFound) = text "Not Found"

currentComponent :: DefinedRoute -> H.ComponentHTML Action ChildSlots Aff
currentComponent Note = slot_ (Proxy :: _ "notes") unit Notes.component unit
currentComponent Checklist = slot_ (Proxy :: _ "checklists") unit Checklists.component unit
currentComponent Signup = slot_ (Proxy :: _ "signup") unit signupComponent unit

tab :: forall w. DefinedRoute -> DefinedRoute -> HTML w Action
tab tabRoute activeRoute =
  div [ class_ "col text-center nav-item px-0" ]
    [ a [ class_ $ "nav-link" <> (if tabRoute == activeRoute then " active" else "")
        , onClick (const $ RouteChanged (Route tabRoute))
        ]
        [ text (tabLabel tabRoute) ]
    ]

tabLabel :: DefinedRoute -> String
tabLabel Note = "Notes"
tabLabel Checklist = "Checklists"
tabLabel Signup = "Signup"

data SignupAction = SignupInitialize | Submit Event | UsernameChanged String | PasswordChanged String
type NoOutput = Void
newtype SignupFormData = SignupFormData SignupState
type SignupState = { username :: String, password :: String }

instance signupFormDataEncodeJson :: EncodeJson SignupFormData where
  encodeJson :: SignupFormData -> Json
  encodeJson (SignupFormData {username, password}) = uname ~> pass ~> jsonEmptyObject
    where uname = "username" := username
          pass = "password" := password

signupInitialState :: SignupState
signupInitialState = { username: "", password: "" }

signupComponent :: forall q i. H.Component q i NoOutput Aff
signupComponent = H.mkComponent { initialState: const signupInitialState
                                , render: signupRender
                                , eval: H.mkEval $ H.defaultEval { handleAction = signupHandleAction
                                                                 , initialize = pure SignupInitialize
                                                                 }
                                }
--(\err -> liftEffect (logShow "Error while trying to signup") >>= const $ pure unit)
--(\r -> liftEffect (logShow r) >>= const $ pure unit)
handleError :: Error -> HalogenM SignupState SignupAction () NoOutput Aff Unit
handleError _ = do
  liftEffect (logShow "Error while trying to signup")
  pure unit

handleResponse :: Response Json -> HalogenM SignupState SignupAction () NoOutput Aff Unit
handleResponse r = do
  let decoded :: Either _ String
      decoded = decodeJson r.body
  liftEffect (logShow decoded)
  pure unit

signupHandleAction :: SignupAction -> HalogenM SignupState SignupAction () NoOutput Aff Unit
signupHandleAction (Submit e) = do
  liftEffect $ preventDefault e
  liftEffect $ logShow "Submission clicked"
  formData <- get
  resp <- liftAff $ post json "/api/signup" $ Just $ Json $ encodeJson $ SignupFormData formData
  either handleError handleResponse resp
signupHandleAction (UsernameChanged newUsername) = do
  liftEffect $ logShow ("New username: " <> newUsername)
  modify_ $ _ { username = newUsername }
signupHandleAction (PasswordChanged newPassword) = do
  liftEffect $ logShow ("New Password: " <> newPassword)
  modify_ $ _ { password = newPassword }
signupHandleAction _ = pure unit

signupRender :: forall m. SignupState -> H.ComponentHTML SignupAction () m
signupRender _ =
  form [] [ label [for "username"] [text "Username"]
  , input [type_ InputText, name "username", placeholder "Enter username", onValueChange UsernameChanged ]
          , label [for "password"] [text "Password"]
          , input [type_ InputPassword, name "password", placeholder "Enter password", onValueChange PasswordChanged]
          , button [ type_ ButtonSubmit, onClick (\e -> Submit (MouseEvent.toEvent e)) ] [text "Submit"]]
