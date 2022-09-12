module Main where

import Prelude

import Affjax.ResponseFormat (json)
import Affjax.Web (get)
import App as App
import Data.Argonaut.Core as Json
import Data.Codec.Argonaut as Codec
import Data.Codec.Argonaut.Record (object)
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
                                                                            (_.body >>> decodeNotesResponse)

  case initialNotes of
    Nothing -> runUI App.component [] body
    Just notes -> runUI App.component (toNote <$> notes) body

toNote :: TmpNote -> App.Note
toNote tmpNote = tmpNote { storageId = Just tmpNote.storageId }

notesCodec :: Codec.JsonCodec (Array TmpNote)
notesCodec = Codec.array $ object "TmpNote" { content: object "content" { noteContent: Codec.string, title: Codec.string }
                                            , storageId: object "storageId" { version: Codec.string, id: Codec.string } }

encodeNotes :: Array TmpNote -> String
encodeNotes notes = Json.stringify (Codec.encode notesCodec notes)

decodeNotes :: Json.Json -> Either Codec.JsonDecodeError (Array TmpNote)
decodeNotes toDecode = Codec.decode notesCodec toDecode

decodeNotesResponse :: Json.Json -> Aff (Maybe (Array TmpNote))
decodeNotesResponse = decodeNotes >>> either (logError >=>| pure Nothing) (Just >>> pure)

logError :: Codec.JsonDecodeError -> Aff Unit
logError = liftEffect <<< logShow

composeBindsIgnoringResult :: forall a b c m. Bind m => (a -> m b) -> m c -> a -> m c
composeBindsIgnoringResult f r i = f i >>= (const r)

infixr 1 composeBindsIgnoringResult as >=>|
