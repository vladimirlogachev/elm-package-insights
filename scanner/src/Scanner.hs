module Scanner (run) where

import Common.Effect
import Common.Env (AppEnv (..))
import Data.Aeson qualified as Aeson
import ElmCli qualified
import ElmJsonCli qualified
import ElmPackage (ElmPackage (..))
import ElmPackage qualified
import ElmPackageApi qualified
import Relude

hardcodedPackages :: [ElmPackage]
hardcodedPackages =
  [ ElmPackage {author = "vladimirlogachev", repo = "elm-modular-grid", fullName = "vladimirlogachev/elm-modular-grid", version = "2.1.0"},
    ElmPackage {author = "jxxcarlson", repo = "elm-stat", fullName = "jxxcarlson/elm-stat", version = "6.0.2"}
  ]

run :: AppM ()
run = do
  ElmCli.checkElmCliAvailable
  ElmJsonCli.checkElmJsonCliAvailable
  putStrLn "Downloading the list of Elm packages..."
  packages <- ElmPackageApi.getPackageList >>= traverse ElmPackage.fromElmPackageSummary . take 2
  ElmCli.initTesting
  failures' <- forM packages $ \package -> do
    ElmCli.initProject package
    ElmJsonCli.installPackage package
    ElmCli.compileProject package
  let failures = catMaybes failures'
  let outputFileName :: Text
      outputFileName = ".package-test/failures.json"
  env <- ask
  fromIO $ Aeson.encodeFile (env.workingDirectory <> "/" <> toString outputFileName) failures
  putTextLn $ "Packages: " <> show (length packages)
  putTextLn $ "Failures: " <> show (length failures)
  putTextLn $ "See " <> show outputFileName <> " for details"
