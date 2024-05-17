module ElmJsonCli (checkElmJsonCliAvailable, installPackage) where

import Common.Effect
import Control.Monad.Except (throwError)
import ElmPackage (ElmPackage (..))
import Relude
import System.Directory (findExecutable)
import System.Exit (ExitCode (ExitSuccess))
import System.Process (CreateProcess (..), proc, readCreateProcessWithExitCode)

getElmJsonCli :: AppM FilePath
getElmJsonCli = do
  res <- liftIO $ findExecutable "elm-json"
  case res of
    Just x -> pure x
    Nothing -> throwError "`elm-json` was not found in PATH. Please install elm-json and try again."

checkElmJsonCliAvailable :: AppM ()
checkElmJsonCliAvailable = void getElmJsonCli

-- TODO: remove, duplicate
{-# INLINE testDirectory #-}
testDirectory :: FilePath
testDirectory = ".package-test"

installPackage :: ElmPackage -> AppM ()
installPackage package = do
  elmJsonCli <- getElmJsonCli
  let elmJsonProcess = (proc elmJsonCli ["install", "--yes", toString package.fullName]) {cwd = Just $ testDirectory <> "/" <> toString package.fullName}
  (code, _stdout', stderr') <- liftIO $ readCreateProcessWithExitCode elmJsonProcess ""
  case code of
    ExitSuccess -> pass
    _ -> throwError $ "Failed to run `elm-json install " <> package.fullName <> "`" <> fromString stderr'
