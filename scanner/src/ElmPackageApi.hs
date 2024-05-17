module ElmPackageApi where

import Data.Aeson qualified as Aeson
import Fmt
import HttpClient qualified
import Relude

elmBaseUrl :: Text
elmBaseUrl = "https://package.elm-lang.org"

getPackageList :: HttpClient.HttpRequestM [ElmPackageSummary]
getPackageList = do
  HttpClient.getDtoPublicWithRetry (elmBaseUrl |++| "/search.json") []

-- Example:
-- {
--   "name": "elm/browser",
--   "summary": "Run Elm in browsers, with access to browser history for single-page apps (SPAs)",
--   "license": "BSD-3-Clause",
--   "version": "1.0.2"
-- },
data ElmPackageSummary = ElmPackageSummary
  { name :: Text,
    version :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Aeson.FromJSON)
