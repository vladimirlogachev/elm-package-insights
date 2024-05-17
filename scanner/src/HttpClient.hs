module HttpClient where

import Common.Effect
import Common.Env (AppEnv)
import Common.QueryParams
import Control.Arrow (left)
import Control.Exception (try)
import Control.Monad.Except (MonadError (catchError, throwError), liftEither, withExceptT)
import Data.Aeson
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as LBS
import Fmt
import Network.HTTP.Conduit (Request (..), httpLbs, newManager, parseRequest, responseBody, tlsManagerSettings)
import Relude

type HttpRequestM a = ERIO AppEnv RequestError a

type RequestError = Text

-- |
-- Get data without the API keys.
-- Usually should not be used directly, use `getDtoPublicWithRetry` instead
getByteStringPublic :: Text -> QueryParams -> HttpRequestM LBS.ByteString
getByteStringPublic path params = do
  response <- fromExceptT . withExceptT transformError . ExceptT . try $ do
    request <- parseRequest (toString fullUrl)
    manager <- newManager tlsManagerSettings
    httpLbs request manager
  -- TODO: add http response trace
  pure $ responseBody response
  where
    fullUrl :: Text
    fullUrl = path |++| optionalParams |+ ""

    optionalParams :: Text
    optionalParams = if null params then "" else "?" +| toQueryString params |+ ""

    transformError :: SomeException -> RequestError
    transformError e = "Failed to fetch " +| fullUrl |+ "\n" +|| e ||+ ""

decodeJsonString :: (FromJSON a) => LBS.ByteString -> HttpRequestM a
decodeJsonString res = liftEither decodeJSON
  where
    decodeJSON :: (FromJSON a) => Either RequestError a
    decodeJSON = eitherDecode res & left handleJSONDecodeError

    handleJSONDecodeError :: String -> RequestError
    handleJSONDecodeError decodeError =
      either
        -- if response is an invalid utf8, then we probably don't care about it.
        (const "Could not decode Utf8")
        -- if response is a valid utf8, then return response body as text, to be able to decode it the other way later
        ( \resString ->
            toText
              $ "decoding error "
              ++ decodeError
              ++ " when decoding response "
              ++ toString resString
        )
        (decodeUtf8Strict @Text @LBS.ByteString res)

-- |
-- Get data without the API keys
getDtoPublicWithRetry :: (FromJSON a) => Text -> QueryParams -> HttpRequestM a
getDtoPublicWithRetry path params = retryingERIO exponentialBackoff7 $ do
  response <- getByteStringPublic path params
  decodeJsonString response
