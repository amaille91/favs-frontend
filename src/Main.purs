module Main where

import Prelude

import Data.Maybe (Maybe(Just, Nothing))
import Data.Either (Either, either)

import Data.Argonaut.Core as Json
import Data.Codec.Argonaut as Codec
import Data.Codec.Argonaut.Record (object)

import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Aff (Aff)
import Effect.Console (logShow)

import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

import Affjax.Web (get)
import Affjax.ResponseFormat (json)

import App as App

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody

  initialNotes :: Maybe (Array App.Note) <- get json "/api/note" >>= either (const $ pure Nothing)
                                                                     (_.body >>> decodeNotesResponse)

  case initialNotes of
    Nothing -> runUI App.component [] body
    Just notes -> runUI App.component notes body

notesCodec :: Codec.JsonCodec (Array App.Note)
notesCodec = Codec.array $ object "Note" { content: object "content" { noteContent: Codec.string, title: Codec.string }
                                         , storageId: object "storageId" { version: Codec.string, id: Codec.string } }

encodeNotes :: Array App.Note -> String
encodeNotes notes = Json.stringify (Codec.encode notesCodec notes)

decodeNotes :: Json.Json -> Either Codec.JsonDecodeError (Array App.Note)
decodeNotes toDecode = Codec.decode notesCodec toDecode

decodeNotesResponse :: Json.Json -> Aff (Maybe (Array App.Note))
decodeNotesResponse = decodeNotes >>> either (logError >=>| pure Nothing) (Just >>> pure)

logError :: Codec.JsonDecodeError -> Aff Unit
logError = liftEffect <<< logShow

composeBindsIgnoringResult :: forall a b c m. Bind m => (a -> m b) -> m c -> a -> m c
composeBindsIgnoringResult f r i = f i >>= (const r)

infixr 1 composeBindsIgnoringResult as >=>|
