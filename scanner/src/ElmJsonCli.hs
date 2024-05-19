module ElmJsonCli (installPackage) where

import Common.Effect
import Common.Env (AppEnv (..))
import ElmPackage (ElmPackage (..))
import Relude
import System.Exit (ExitCode (ExitSuccess))
import System.Process (CreateProcess (..), proc, readCreateProcessWithExitCode)

installPackage :: ElmPackage -> AppM (Either Text ())
installPackage package = do
  env <- ask
  let elmJsonProcess = (proc env.elmJsonExecutable ["install", "--yes", toString package.fullName]) {cwd = Just $ env.testDirectory <> "/" <> toString package.fullName}
  (code, _stdout', stderr') <- liftIO $ readCreateProcessWithExitCode elmJsonProcess ""
  case code of
    ExitSuccess -> pure (Right ())
    _ -> pure $ Left $ fromString stderr'
