module Main where

import Prelude

import Affjax.ResponseFormat (json)
import Affjax.Web (get)
import App (decodeNotesResponse) as App 
import Data.Either (Either, either)
import Data.Maybe (Maybe(Just, Nothing))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (logShow)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

type TmpNote = { content :: { noteContent :: String, title :: String }
               , storageId :: { version :: String, id :: String }}
main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody

  initialNotes :: Maybe (Array TmpNote) <- get json "/api/note" >>= either (const $ pure Nothing)
                                                                            (_.body >>> App.decodeNotesResponse logError)

  case initialNotes of
    Nothing -> runUI App.component [] body
    Just notes -> runUI App.component (toNote <$> notes) body

logError :: forall a. Show a => a -> Aff Unit
logError = liftEffect <<< logShow

