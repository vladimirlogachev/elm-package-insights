module Common.Env (AppEnv (..), mkAppEnv) where

import Relude
import System.Directory (getCurrentDirectory)

newtype AppEnv = AppEnv
  { workingDirectory :: FilePath
  }

mkAppEnv :: IO AppEnv
mkAppEnv = do
  wd <- getCurrentDirectory
  pure AppEnv {workingDirectory = wd}
