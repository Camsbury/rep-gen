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
  ( cons
  , index
  , snoc
  , uncons
  , unsnoc
  , Index
  )
import Control.Monad.Except (MonadError(..))
import Data.Default
import Network.HTTP.Types.Status
import Network.HTTP.Types.Method
import Network.HTTP.Types.URI
import Text.Pretty.Simple hiding (Color(..))
import qualified Network.HTTP.Client as H
import qualified Network.HTTP.Client.TLS as H
import qualified Data.ByteString as B

-- | throws error if Nothing
throwMaybe :: MonadError e m => e -> Maybe a -> m a
throwMaybe e = maybe (throwError e) pure

-- | throws error if Nothing
throwEither :: MonadError e m => Either e a -> m a
throwEither = either throwError pure

type Url = Text

getRequest
  :: Url
  -> Map Text Text
  -> IO (Int, Text)
getRequest url queryParams = do
  manager <- H.newManager H.tlsManagerSettings
  request <- H.parseRequest . unpack $ url
  let mapToBS (k, v) = (encodeUtf8 k, Just $ encodeUtf8 v)
  let bsParams
        =   queryParams
        ^.. itraversed
        .   withIndex
        .   to mapToBS
  let requestWithParams = H.setQueryString bsParams request
  response <- H.httpLbs requestWithParams manager
  let code = statusCode $ H.responseStatus response
  let responseBody = tshow $ H.responseBody response
  pure (code, responseBody)

(/?) :: (Eq a, Fractional a) => a -> a -> Maybe a
_ /? 0 = Nothing
x /? y = Just (x / y)
