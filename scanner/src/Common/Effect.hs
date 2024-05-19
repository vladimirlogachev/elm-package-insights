module Common.Effect where

import Common.Env
import Control.Concurrent.Async (mapConcurrently)
import Control.Monad.Except (liftEither)
import Control.Retry
import Relude

-- |
-- Note: When using this type as a type argument for some Conduit, you may end up with the following error:
-- `The type synonym ‘AppM’ should have 1 argument, but has been given none`.
-- In this case, you may simply replace AppM with its definition. 🤷🏻‍♂️
type AppM a = ExceptT AppError (ReaderT AppEnv IO) a

type AppError = Text

type ERIO r e a = ExceptT e (ReaderT r IO) a

{-# INLINE fromExceptT #-}
fromExceptT :: ExceptT e IO a -> ERIO r e a
fromExceptT = ExceptT . ReaderT . const . runExceptT

{-# INLINE toExceptT #-}
toExceptT :: r -> ERIO r e a -> ExceptT e IO a
toExceptT r expr = ExceptT $ usingReaderT r (runExceptT expr)

{-# INLINE fromEitherIO #-}
fromEitherIO :: IO (Either e a) -> ERIO r e a
fromEitherIO = ExceptT . ReaderT . const

-- TODO: require an error handling function in arguments?
{-# INLINE fromIO #-}
fromIO :: IO a -> ERIO r e a
fromIO = fromEitherIO . fmap Right

{-# INLINE toEitherIO #-}
toEitherIO :: r -> ERIO r e a -> IO (Either e a)
toEitherIO r expr = usingReaderT r (runExceptT expr)

-- |
-- Expects all to be successful
{-# INLINE mapConcurrentlyERIO #-}
mapConcurrentlyERIO :: (Traversable t) => (a -> ERIO r e b) -> t a -> ERIO r e (t b)
mapConcurrentlyERIO f inputs = do
  env <- ask
  eithers <- liftIO $ mapConcurrently (toEitherIO env . f) inputs
  liftEither $ sequence eithers

-- |
-- Note: The baseDelay is in *MICROseconds*
-- Max delay (excluding the time for the expression to fail) is:
-- 50 + 100 + 200 + 400 + 800 + 1600 + 3200 = 6300 ms
exponentialBackoff7 :: (Monad m) => RetryPolicyM m
exponentialBackoff7 = exponentialBackoff 50_000 <> limitRetries 7

-- |
-- The simplest retry helper for ExceptT, does not provide a way to check the result.
-- Useful for idempotent queries
retryingExceptT :: RetryPolicyM IO -> ExceptT e IO a -> ExceptT e IO a
retryingExceptT policy expr =
  ExceptT $ retrying policy (\_status x -> pure $ isLeft x) (\_status -> runExceptT expr)

-- |
-- The simplest retry helper for ERIO, does not provide a way to check the result.
-- Useful for idempotent queries
retryingERIO :: RetryPolicyM IO -> ERIO r e a -> ERIO r e a
retryingERIO policy expr = do
  appEnv <- ask
  fromExceptT $ retryingExceptT policy $ toExceptT appEnv expr
