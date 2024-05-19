module Main (main) where

import Common.Effect
import Common.Env
import Options.Applicative
import Relude
import Scanner (ScannerProps (..))
import Scanner qualified
import System.Exit (ExitCode (ExitFailure))

main :: IO ()
main = do
  env <- mkAppEnv
  props <- customExecParser (prefs showHelpOnError) parserInfo
  res <- toEitherIO env $ Scanner.run props
  case res of
    Left err -> do
      putTextLn $ "Error: " <> err
      exitWith $ ExitFailure 1
    Right _ -> exitSuccess
  where
    parserInfo :: ParserInfo ScannerProps
    parserInfo =
      info
        (helper <*> scannerPropsParser)
        (fullDesc <> progDesc "Elm Package Insights")

    scannerPropsParser :: Parser ScannerProps
    scannerPropsParser =
      ScannerProps
        <$> switch
          ( long "reset-elm-packages-cache"
              <> help "Reset local elm packages cache (required for reliable test, will slow down the test a lot)."
              <> showDefault
          )
        <*> switch
          ( long "reset-test-directories"
              <> help "Remove test directories before running tests (shouldn't affect the test much, and will slow down the test)."
              <> showDefault
          )
