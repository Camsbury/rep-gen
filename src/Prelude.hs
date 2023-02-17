module Prelude
  ( module ClassyPrelude
  , module Text.Pretty.Simple
  , module Control.Lens.Operators
  , module Control.Lens.Combinators
  , module Data.Default
  , module Prelude
  ) where

import ClassyPrelude

import Control.Lens.Operators hiding ((<|), (<.>))
import Control.Lens.Combinators hiding
  ( children
  , cons
  , index
  , snoc
  , uncons
  , unsnoc
  , Index
  )
import Control.Monad.Except (MonadError(..))
import Data.Default
import Text.Pretty.Simple hiding (Color(..))

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
