module Scanner (run) where

import Common.Effect
import Common.Env (AppEnv (..))
import Data.Aeson qualified as Aeson
import Data.List.Split qualified as List.Split
import ElmCli (FailedCompilation (..), FailureReason (..))
import ElmCli qualified
import ElmJsonCli qualified
import ElmPackage (ElmPackage (..))
import ElmPackage qualified
import ElmPackageApi qualified
import Relude

hardcodedPackages :: [ElmPackage]
hardcodedPackages =
  [ ElmPackage {author = "vladimirlogachev", repo = "elm-modular-grid", fullName = "vladimirlogachev/elm-modular-grid", version = "2.1.0"},
    ElmPackage {author = "jxxcarlson", repo = "elm-stat", fullName = "jxxcarlson/elm-stat", version = "6.0.2"},
    ElmPackage {author = "Skinney", repo = "elm-warrior", fullName = "Skinney/elm-warrior", version = "4.0.5"},
    ElmPackage {author = "IzumiSy", repo = "elm-consistent-hashing", fullName = "IzumiSy/elm-consistent-hashing", version = "4.0.5"}
  ]

checkPackage :: ElmPackage -> AppM (Maybe FailedCompilation)
checkPackage package = do
  ElmCli.initProject package
  res <- ElmJsonCli.installPackage package
  case res of
    Right _ -> ElmCli.compileProject package
    Left err -> pure $ Just $ FailedCompilation {package = package, reason = InstallationFailed err}

run :: AppM ()
run = do
  putStrLn "Downloading the list of Elm packages..."
  packages <- ElmPackageApi.getPackageList >>= traverse ElmPackage.fromElmPackageSummary
  -- let packages = hardcodedPackages -- Note: for local quick testing
  putStrLn "Download complete."
  ElmCli.resetElmPackagesCache -- TODO: add a flag, use optparse-applicative
  ElmCli.resetDirectoriesAndCreateStubProject -- TODO: add a flag, use optparse-applicative
  let groupSize :: Int
      groupSize = 20

  let checkGroup (i, g) = do
        putTextLn $ "Processing items " <> show (i * groupSize) <> " to " <> show (i * groupSize + length g - 1) <> "..."
        failures <- catMaybes <$> mapConcurrentlyERIO checkPackage g
        unless (null failures) $ do
          putTextLn $ "  Failures: " <> show (length failures)
        pure failures

  failures' <-
    List.Split.chunksOf groupSize packages
      & zip [0 ..]
      & traverse checkGroup

  let failures = concat failures' & sortOn (ElmPackage.fullName . ElmCli.package)
  env <- ask
  let detailedFileName = env.outputsDirectory <> "/failures-detailed.json"
      simpleFileName = env.outputsDirectory <> "/failures.txt"
  fromIO $ Aeson.encodeFile detailedFileName failures
  fromIO $ writeFileText simpleFileName $ unlines $ map (ElmPackage.fullName . ElmCli.package) failures
  putTextLn $ "Packages: " <> show (length packages)
  putTextLn $ "Failures: " <> show (length failures)
  putTextLn $ "See " <> fromString simpleFileName <> " and " <> fromString detailedFileName <> " for details"
