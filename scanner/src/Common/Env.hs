module Common.Env (AppEnv, mkAppEnv) where

import Relude

data AppEnv = AppEnv

mkAppEnv :: IO AppEnv
mkAppEnv = pure AppEnv
