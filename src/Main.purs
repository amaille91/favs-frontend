module Main where

import Prelude

import Affjax.ResponseFormat (json)
import Affjax.Web (Error, get, printError)
import App (Note)
import App (component) as App
import Data.Argonaut.Decode (JsonDecodeError, decodeJson)
import Data.Bifunctor (lmap)
import Data.Either (Either, either)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (logShow)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

type TmpNote = { content :: { noteContent :: String, title :: String }
               , storageId :: { version :: String, id :: String }}

data FatalError = DecodeError JsonDecodeError
                | NetworkError Error

instance fatalErrorShowInstance :: Show FatalError where
  show (DecodeError err) = "DecodeError" <> show err
  show (NetworkError err) = "NetworkError" <> printError err

instance jsonDecodeErrorToFatalErrorInstance :: ToFatalError JsonDecodeError where
  toFatalError = DecodeError

instance affjaxErrorToFatalErrorInstance :: ToFatalError Error where
  toFatalError = NetworkError

class ToFatalError a where
  toFatalError :: a -> FatalError

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody

  initialNotes :: Either FatalError (Array Note) <- get json "/api/note" >>= (\ei -> pure $ lmap toFatalError ei >>= (_.body >>> (decodeJson >>> lmap toFatalError)))


 -- >>= (either pure (_.body >>> decodeJson >>> lmap toFatalError >>> pure))
  either (logError >>> (const $ runUI App.component [] body))
         (\(serverNotes :: Array Note) -> runUI App.component serverNotes body)
         initialNotes

logError :: forall a. Show a => a -> Aff Unit
logError = liftEffect <<< logShow

