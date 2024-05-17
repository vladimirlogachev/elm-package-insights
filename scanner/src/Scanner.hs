module Scanner (run) where

import Common.Effect
import Data.Aeson qualified as Aeson
import Data.Text qualified as Text
import ElmCli qualified
import ElmJsonCli qualified
import ElmPackage (ElmPackage (..))
import ElmPackage qualified
import ElmPackageApi qualified
import Relude
import System.Directory (getCurrentDirectory)
import Text.Pretty.Simple (pPrint)

run :: AppM ()
run = do
  currentDirectory <- fromIO getCurrentDirectory
  ElmCli.checkElmCliAvailable
  ElmJsonCli.checkElmJsonCliAvailable
  putStrLn "Downloading the list of Elm packages..."
  packages <- ElmPackageApi.getPackageList >>= traverse ElmPackage.fromElmPackageSummary
  -- let packages =
  --       [ ElmPackage {author = "vladimirlogachev", repo = "elm-modular-grid", fullName = "vladimirlogachev/elm-modular-grid", version = "2.1.0"},
  --         ElmPackage {author = "jxxcarlson", repo = "elm-stat", fullName = "jxxcarlson/elm-stat", version = "6.0.2"}
  --       ]
  ElmCli.initTesting
  failures' <- forM (take 100 packages) $ \package -> do
    ElmCli.initProject package
    ElmJsonCli.installPackage package
    ElmCli.compileProject package
  let failures = catMaybes failures'
  let outputFileName :: Text
      outputFileName = ".package-test/failures.json"
  fromIO $ Aeson.encodeFile (currentDirectory <> "/" <> toString outputFileName) failures
  putTextLn $ "Packages: " <> show (length packages)
  putTextLn $ "Failures: " <> show (length failures)
  putTextLn $ "See " <> show outputFileName <> " for details"
