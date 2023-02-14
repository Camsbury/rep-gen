module Prelude
  ( module ClassyPrelude
  , getRequest
  ) where

import ClassyPrelude

import Control.Lens.Operators
import Control.Lens.Combinators
import Network.HTTP.Types.Method
import Network.HTTP.Types.URI
import qualified Network.HTTP.Client as H
import qualified Network.HTTP.Client.TLS as H
import qualified Data.ByteString as B

type Url = Text

getRequest
  :: Url
  -> Map Text Text
  -> IO Text
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
  pure . tshow $ H.responseBody response

  --   let statusCode = H.statusCode $ H.responseStatus response
  -- let body = H.responseBody response
  -- pure (statusCode, body)
