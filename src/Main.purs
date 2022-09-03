module Main where

import Prelude

import Data.Maybe (Maybe(Just, Nothing))
import Data.Either (Either(Left, Right), either)
import Data.List.Types (List)

import Data.Argonaut.Core as Json
import Data.Codec.Argonaut as Codec
import Data.Codec.Argonaut.Record (object)
import Data.List (fromFoldable)

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

  eitherResp <- get json "/api/note"

  initialNotes :: Maybe (List App.Note) <- case eitherResp of
    Left _ -> pure Nothing
    Right resp ->  either logError (\r -> pure (Just r)) $ fromFoldable <$> decodeNotes resp.body

  liftEffect $ logShow initialNotes
  liftEffect $ logShow "notes shown"

  case initialNotes of
    Nothing -> runUI App.component Nothing body
    Just notes -> runUI App.component (Just { notes: notes }) body

notesCodec :: Codec.JsonCodec (Array App.Note)
notesCodec = Codec.array $ object "Note" { content: object "content" { noteContent: Codec.string, title: Codec.string }
                                         , storageId: object "storageId" { version: Codec.string, id: Codec.string } }

encodeNotes :: Array App.Note -> String
encodeNotes notes = Json.stringify (Codec.encode notesCodec notes)

decodeNotes :: Json.Json -> Either Codec.JsonDecodeError (Array App.Note)
decodeNotes toDecode = Codec.decode notesCodec toDecode

logError :: Codec.JsonDecodeError -> Aff (Maybe (List App.Note))
logError e = do
  liftEffect $ logShow e
  pure Nothing
