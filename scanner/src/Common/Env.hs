module Common.Env (AppEnv (..), mkAppEnv) where

import Relude
import System.Directory
  ( createDirectoryIfMissing,
    findExecutable,
    getCurrentDirectory,
    getHomeDirectory,
  )

data AppEnv = AppEnv
  { elmCachePath :: FilePath,
    testDirectory :: FilePath,
    stubProjectDirectory :: FilePath,
    outputsDirectory :: FilePath,
    elmExecutable :: FilePath,
    elmJsonExecutable :: FilePath
  }

mkAppEnv :: IO AppEnv
mkAppEnv = do
  homeDir <- getHomeDirectory
  workingDirectory <- getCurrentDirectory
  elmExecutable <- getElmExecutable
  elmJsonExecutable <- getElmJsonExecutable
  let testDirectory = workingDirectory <> "/.package-test"
      env =
        AppEnv
          { elmCachePath = homeDir <> "/.elm/0.19.1/packages",
            testDirectory = testDirectory,
            stubProjectDirectory = testDirectory <> "/.stub-project",
            outputsDirectory = testDirectory <> "/.outputs",
            elmExecutable = elmExecutable,
            elmJsonExecutable = elmJsonExecutable
          }
  createDirectoriesIfMissing env
  pure env

getElmExecutable :: IO FilePath
getElmExecutable = do
  res <- findExecutable "elm"
  case res of
    Just x -> pure x
    Nothing -> error "`elm` was not found in PATH. Please install Elm and try again."

getElmJsonExecutable :: IO FilePath
getElmJsonExecutable = do
  res <- findExecutable "elm-json"
  case res of
    Just x -> pure x
    Nothing -> error "`elm-json` was not found in PATH. Please install `elm-json` and try again."

createDirectoriesIfMissing :: AppEnv -> IO ()
createDirectoriesIfMissing env = do
  createDirectoryIfMissing True env.testDirectory
  createDirectoryIfMissing True env.stubProjectDirectory
  createDirectoryIfMissing True env.outputsDirectory
