{-# OPTIONS_GHC -Wno-orphans #-}
--------------------------------------------------------------------------------
module Prelude
  ( module ClassyPrelude
  , module Text.Pretty.Simple
  , module Control.Lens.Operators
  , module Control.Lens.Combinators
  , module Data.Default
  , module Prelude
  , LogLevel(..)
  , MonadError(..)
  , MonadState(..)
  , MonadLogger(..)
  , command
  , command_
  , evalStateT
  , filterLogger
  , logDebugN
  , logErrorN
  , logInfoN
  , logWarnN
  , runExceptT
  , runStateT
  , runStdoutLoggingT
  ) where
--------------------------------------------------------------------------------
import ClassyPrelude
--------------------------------------------------------------------------------
import Control.Lens.Operators hiding ((<|), (<.>))
import Control.Lens.Combinators hiding
  ( children
  , cons
  , index
  , sans
  , snoc
  , uncons
  , unsnoc
  , Index
  )
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Logger
import Data.Default
import System.Command (command, command_)
import Text.Pretty.Simple hiding (Color(..))
--------------------------------------------------------------------------------
import Data.Aeson
  ( FromJSON(..)
  )
--------------------------------------------------------------------------------
import qualified Data.Aeson as J
import qualified Data.Aeson.Types as J
--------------------------------------------------------------------------------

-- | throws error if Nothing
throwMaybe :: MonadError e m => e -> Maybe a -> m a
throwMaybe e = maybe (throwError e) pure

-- | throws error if Nothing
throwEither :: MonadError e m => Either e a -> m a
throwEither = either throwError pure

type Url = Text

(/?) :: (Eq a, Fractional a) => a -> a -> Maybe a
_ /? 0 = Nothing
x /? y = Just (x / y)

-- | Use the thing if true
toMaybe :: Bool -> a -> Maybe a
toMaybe p v =
  if p then Just v else Nothing

(/.) :: (Integral a, Fractional b) => a -> a -> b
a /. b = fromIntegral a / fromIntegral b

-- | Nested functor map
f2map :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
f2map f = fmap (fmap f)

-- | Straight outta GHC, just apply a function if true
applyWhen :: Bool -> (a -> a) -> a -> a
applyWhen p f a = if p then f a else a

instance FromJSON LogLevel where
  parseJSON = J.withText "LogLevel" $ \t -> case toLower t of
    "debug" -> pure      LevelDebug
    "info"  -> pure      LevelInfo
    "warn"  -> pure      LevelWarn
    "error" -> pure      LevelError
    _       -> J.parseFail "Invalid log level"

