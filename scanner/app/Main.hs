module Main (main) where

import Common.Effect
import Common.Env
import Relude
import Scanner qualified
import System.Exit (ExitCode (ExitFailure))

main :: IO ()
main = do
  env <- mkAppEnv
  res <- toEitherIO env Scanner.run
  case res of
    Left err -> do
      putTextLn $ "Error: " <> err
      exitWith $ ExitFailure 1
    Right _ -> do
      putTextLn "Success"
      exitSuccess
