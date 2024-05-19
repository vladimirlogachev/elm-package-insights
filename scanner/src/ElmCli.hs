module ElmCli
  ( initProject,
    resetElmPackagesCache,
    resetTestDirectories,
    createStubProject,
    compileProject,
    FailedCompilation (..),
    FailureReason (..),
  )
where

import Common.Effect
import Common.Env (AppEnv (..))
import Control.Monad.Except (throwError)
import Data.Aeson ((.:))
import Data.Aeson qualified as Aeson
import Data.Text qualified as Text
import ElmPackage (ElmPackage (..))
import FileSystem qualified
import Relude
import System.Directory (copyFile, createDirectoryIfMissing)
import System.Exit (ExitCode (ExitSuccess))
import System.Process (CreateProcess (..), proc, readCreateProcessWithExitCode)

-- | Optional
resetElmPackagesCache :: AppM ()
resetElmPackagesCache = do
  env <- ask
  putTextLn "Cleaning Elm package cache..."
  FileSystem.recursivelyDeleteDirectory env.elmCachePath

-- | Optional
resetTestDirectories :: AppM ()
resetTestDirectories = do
  env <- ask
  FileSystem.recursivelyDeleteDirectory env.testDirectory
  FileSystem.recursivelyDeleteDirectory env.outputsDirectory
  fromIO $ createDirectoryIfMissing True env.testDirectory
  fromIO $ createDirectoryIfMissing True env.outputsDirectory

-- | Required
createStubProject :: AppM ()
createStubProject = do
  env <- ask
  FileSystem.recursivelyDeleteDirectory env.stubProjectDirectory
  fromIO $ createDirectoryIfMissing True env.stubProjectDirectory
  let elmInitProcess = (proc env.elmExecutable ["init"]) {cwd = Just env.stubProjectDirectory}
  (code, _stdout', stderr') <- liftIO $ readCreateProcessWithExitCode elmInitProcess "y\n"
  unless (null stderr') $ putStrLn stderr'
  case code of
    ExitSuccess -> pass
    _ -> throwError "Failed to run `elm init`"
  fromIO $ writeFileText (env.stubProjectDirectory <> "/src/Main.elm") stubElmMainFile

stubElmMainFile :: Text
stubElmMainFile =
  Text.unlines
    [ "module Main exposing (main)",
      "import Html",
      "main = Html.text \"\""
    ]

packageDirectory :: FilePath -> ElmPackage -> FilePath
packageDirectory testDirectory package = testDirectory <> "/" <> toString package.fullName

-- |
-- Create a directory for testing a package
createSrcDirectoryForPackage :: ElmPackage -> AppM ()
createSrcDirectoryForPackage package = do
  env <- ask
  fromIO $ createDirectoryIfMissing True (packageDirectory env.testDirectory package <> "/src")

initProject :: ElmPackage -> AppM ()
initProject package = do
  env <- ask
  createSrcDirectoryForPackage package
  fromIO $ copyFile (env.stubProjectDirectory <> "/src/Main.elm") (packageDirectory env.testDirectory package <> "/src/Main.elm")
  fromIO $ copyFile (env.stubProjectDirectory <> "/elm.json") (packageDirectory env.testDirectory package <> "/elm.json")

compileProject :: ElmPackage -> AppM (Maybe FailedCompilation)
compileProject package = do
  env <- ask
  let elmMakeProcess = (proc env.elmExecutable ["make", "--output=/dev/null", "--report=json", "src/Main.elm"]) {cwd = Just (packageDirectory env.testDirectory package)}
  (code, _stdout', stdErrJson) <- fromIO $ readCreateProcessWithExitCode elmMakeProcess ""
  case code of
    ExitSuccess -> pure Nothing
    _ -> do
      case Aeson.decodeStrict' @AsCorruptPackageData (fromString stdErrJson) of
        Just (AsCorruptPackageData problemPackage) -> pure $ Just $ FailedCompilation {package = package, reason = CorruptPackageData problemPackage}
        Nothing -> do
          case Aeson.decodeStrict' @AsProblemDownloadingPackage (fromString stdErrJson) of
            Just AsProblemDownloadingPackage -> pure $ Just $ FailedCompilation {package = package, reason = ProblemDownloadingPackage}
            Nothing -> pure $ Just $ FailedCompilation {package = package, reason = OtherReason $ fromString stdErrJson}

data FailedCompilation = FailedCompilation
  { package :: ElmPackage,
    reason :: FailureReason
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Aeson.ToJSON)

data FailureReason
  = CorruptPackageData ElmPackage
  | ProblemDownloadingPackage
  | InstallationFailed Text
  | OtherReason Text
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Aeson.ToJSON)

-- Example:
-- {
--   "type": "error",
--   "path": null,
--   "title": "CORRUPT PACKAGE DATA",
--   "message": [
--     "I downloaded the source code for ryannhg/date-format 2.3.0 from:\n\n    ",
--     {
--       "bold": false,
--       "underline": false,
--       "color": "yellow",
--       "string": "https://github.com/ryannhg/date-format/zipball/2.3.0/"
--     },
--     "\n\nBut it looks like the hash of the archive has changed since publication:\n\n  Expected: 70c67866fed499bec685f43f23fea279556757f2\n    Actual: 86534146f5a550bb8e87b87a7484ea5732090bb5\n\nThis usually means that the package author moved the version tag, so report it\nto them and see if that is the issue. Folks on Elm slack can probably help as\nwell."
--   ]
-- }
newtype AsCorruptPackageData = AsCorruptPackageData ElmPackage

instance Aeson.FromJSON AsCorruptPackageData where
  parseJSON = Aeson.withObject "AsCorruptPackageData" $ \o -> do
    type' <- o .: "type"
    title <- o .: "title"
    guard (type' == Aeson.String "error" && title == Aeson.String "CORRUPT PACKAGE DATA")
    message <- o .: "message"
    case message of
      Aeson.String firstString : _ ->
        case parseProblematicPackageName firstString of
          Just x -> pure $ AsCorruptPackageData x
          Nothing -> fail "Expected a package name in the message field"
      _ -> fail "Expected a single string in the message field"

-- |
-- Input:
-- "I downloaded the source code for ryannhg/date-format 2.3.0 from:"
-- Output :
-- ElmPackage
--   { author = "ryannhg",
--     repo = "date-format",
--     fullName = "ryannhg/date-format",
--     version = "2.3.0"
--   }
parseProblematicPackageName :: Text -> Maybe ElmPackage
parseProblematicPackageName s =
  let parts = Text.words s
   in case parts of
        "I" : "downloaded" : "the" : "source" : "code" : "for" : fullName : version : "from:" : _ ->
          case Text.splitOn "/" fullName of
            [author, repo] -> Just $ ElmPackage {author = author, repo = repo, fullName = fullName, version = version}
            _ -> Nothing
        _ -> Nothing

-- Example 2:
-- {
--   "type": "error",
--   "path": "elm.json",
--   "title": "INCOMPATIBLE DEPENDENCIES",
--   "message": [
--     "The dependencies in your elm.json are not compatible.\n\nDid you change them by hand? Try to change it back! It is much more reliable to\nadd dependencies with ",
--     {
--       "bold": false,
--       "underline": false,
--       "color": "GREEN",
--       "string": "elm install"
--     },
--     " or the dependency management tool in\n",
--     {
--       "bold": false,
--       "underline": false,
--       "color": "GREEN",
--       "string": "elm reactor"
--     },
--     ".\n\nPlease ask for help on the community forums if you try those paths and are still\nhaving problems!"
--   ]
-- }

-- Example 3:
-- "contents": "{\"type\":\"error\",\"path\":null,\"title\":\"PROBLEM DOWNLOADING PACKAGE\",\"message\":[\"I was trying to download the source code for Skinney/elm-warrior 4.0.5, so I\\ntried to fetch:\\n\\n    \",{\"bold\":false,\"underline\":false,\"color\":\"yellow\",\"string\":\"https://github.com/Skinney/elm-warrior/zipball/4.0.5/\"},\"\\n\\nBut it came back as \",{\"bold\":false,\"underline\":false,\"color\":\"RED\",\"string\":\"404\"},\" Not Found\\n\\nThis may mean some online endpoint changed in an unexpected way, so if does not\\nseem like something on your side is causing this (e.g. firewall) please report\\nthis to https://github.com/elm/compiler/issues with your operating system, Elm\\nversion, the command you ran, the terminal output, and any additional\\ninformation that can help others reproduce the error!\"]}",
data AsProblemDownloadingPackage = AsProblemDownloadingPackage

instance Aeson.FromJSON AsProblemDownloadingPackage where
  parseJSON = Aeson.withObject "AsProblemDownloadingPackage" $ \o -> do
    type' <- o .: "type"
    title <- o .: "title"
    guard (type' == Aeson.String "error" && title == Aeson.String "PROBLEM DOWNLOADING PACKAGE")
    pure AsProblemDownloadingPackage
