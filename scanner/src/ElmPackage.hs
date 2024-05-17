module ElmPackage where

import Control.Monad.Except (MonadError (throwError))
import Data.Aeson qualified as Aeson
import Data.Text qualified as Text
import ElmPackageApi (ElmPackageSummary (..))
import Relude

data ElmPackage = ElmPackage
  { author :: Text, -- "elm"
    repo :: Text, -- "browser"
    fullName :: Text, -- "elm/browser"
    version :: Text -- "1.0.2"
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Aeson.ToJSON)

{-# INLINE fromElmPackageSummary #-}
fromElmPackageSummary :: (MonadError Text m) => ElmPackageSummary -> m ElmPackage
fromElmPackageSummary summary =
  case Text.splitOn "/" summary.name of
    [a, p] ->
      pure
        $ ElmPackage
          { author = a,
            repo = p,
            fullName = summary.name,
            version = summary.version
          }
    _ -> throwError $ "Invalid package name: " <> summary.name
