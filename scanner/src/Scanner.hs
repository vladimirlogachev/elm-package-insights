module Scanner (run, ScannerProps (..)) where

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

data ScannerProps = ScannerProps
  { resetElmPackagesCache :: Bool,
    resetTestDirectories :: Bool
  }

run :: ScannerProps -> AppM ()
run props = do
  putStrLn "Downloading the list of Elm packages..."
  packages <- ElmPackageApi.getPackageList >>= traverse ElmPackage.fromElmPackageSummary
  -- let packages = shortPackageList
  putStrLn $ "Download complete, " <> show (length packages) <> " packages"
  when props.resetElmPackagesCache ElmCli.resetElmPackagesCache
  when props.resetTestDirectories ElmCli.resetTestDirectories
  ElmCli.createStubProject
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
  let detailedFileName = env.outputsDirectory <> "/broken-packages.json"
      simpleFileName = env.outputsDirectory <> "/broken-packages.txt"
  fromIO $ Aeson.encodeFile detailedFileName failures
  fromIO $ writeFileText simpleFileName $ unlines $ map (ElmPackage.fullName . ElmCli.package) failures
  putTextLn ""
  putTextLn "Done!"
  putTextLn ""
  putTextLn $ "Total packages: " <> show (length packages)
  putTextLn $ "Broken packages: " <> show (length failures)
  putTextLn ""
  putTextLn $ "Broken package list: " <> fromString simpleFileName
  putTextLn $ "Details: " <> fromString detailedFileName

checkPackage :: ElmPackage -> AppM (Maybe FailedCompilation)
checkPackage package = do
  ElmCli.initProject package
  res <- ElmJsonCli.installPackage package
  case res of
    Right _ -> ElmCli.compileProject package
    Left err -> pure $ Just $ FailedCompilation {package = package, reason = InstallationFailed err}

-- | Note: short list for local quick testing
-- TODO: move to integration tests
shortPackageList :: [ElmPackage]
shortPackageList =
  [ ElmPackage {author = "vladimirlogachev", repo = "elm-modular-grid", fullName = "vladimirlogachev/elm-modular-grid", version = "2.1.0"},
    ElmPackage {author = "jxxcarlson", repo = "elm-stat", fullName = "jxxcarlson/elm-stat", version = "6.0.2"},
    ElmPackage {author = "Skinney", repo = "elm-warrior", fullName = "Skinney/elm-warrior", version = "4.0.5"},
    ElmPackage {author = "IzumiSy", repo = "elm-consistent-hashing", fullName = "IzumiSy/elm-consistent-hashing", version = "4.0.5"}
  ]
