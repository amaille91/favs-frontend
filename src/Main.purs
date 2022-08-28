module Main where

import Prelude

import Data.Maybe (Maybe(Just, Nothing))
import Data.Either (Either(Left, Right), hush)
import Data.List.Types (List)

import Data.Argonaut.Core as Json
import Data.Codec.Argonaut as Codec
import Data.Codec.Argonaut.Record (object)
import Data.Argonaut.Parser as Parser
import Data.List (fromFoldable)

import Effect (Effect)

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
    Right resp -> pure $ hush $ fromFoldable <$> decodeNotes resp.body

  case initialNotes of
    Nothing -> runUI App.component Nothing body
    Just notes -> runUI App.component (Just { notes: notes }) body

notesCodec :: Codec.JsonCodec (Array App.Note)
notesCodec = Codec.array $ object "Note" { name: Codec.string }

encodeNotes :: Array App.Note -> String
encodeNotes notes = Json.stringify (Codec.encode notesCodec notes)

decodeNotes :: Json.Json -> Either Codec.JsonDecodeError (Array App.Note)
decodeNotes toDecode = Codec.decode notesCodec toDecode

parseJson :: String -> Either String Json.Json
parseJson toDecode = Parser.jsonParser toDecode

data JsonDecodingError = JsonDecodeError Codec.JsonDecodeError | JsonParsingError String
