module Prelude
  ( module ClassyPrelude
  , module Text.Pretty.Simple
  , module Control.Lens.Operators
  , module Control.Lens.Combinators
  , module Data.Default
  , getRequest
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
import Data.Default
import Network.HTTP.Types.Status
import Network.HTTP.Types.Method
import Network.HTTP.Types.URI
import Text.Pretty.Simple hiding (Color(..))
import qualified Network.HTTP.Client as H
import qualified Network.HTTP.Client.TLS as H
import qualified Data.ByteString as B

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
