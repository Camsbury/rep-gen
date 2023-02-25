{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveAnyClass #-}
--------------------------------------------------------------------------------
module Web
  ( getRequest
  , cachedGetRequest
  , migrateAll
  ) where
--------------------------------------------------------------------------------
import Network.HTTP.Types.Status (Status(..))
import Network.HTTP.Types.Method ()
import Network.HTTP.Types.URI ()
import Database.Persist (Entity(..), (==.))
import Database.Persist.TH (mkPersist, mkMigrate, persistLowerCase, share, sqlSettings)
import Database.Persist.Sqlite (runSqlite)
--------------------------------------------------------------------------------
import qualified Network.HTTP.Client as H
import qualified Network.HTTP.Client.TLS as H
import qualified Database.Persist as DP
--------------------------------------------------------------------------------

getRequest
  :: Url
  -> Map Text Text
  -> IO (Int, ByteString)
getRequest url queryParams = do
  manager <- H.newManager H.tlsManagerSettings
  request <- H.parseRequest . unpack $ url
  let mapToBS (k, v) = (encodeUtf8 k, Just $ encodeUtf8 v)
  let bsParams
        = queryParams
        ^.. itraversed
        . withIndex
        . to mapToBS
  let requestWithParams = H.setQueryString bsParams request
  response <- H.httpLbs requestWithParams manager
  let code = statusCode $ H.responseStatus response
  let responseBody = toStrict $ H.responseBody response
  pure (code, responseBody)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Cache
    key Text
    response ByteString
    deriving Eq Show
|]

mkCacheKey :: Text -> Map Text Text -> Text
mkCacheKey url qps = "cache:" <> url <> ":" <> tshow qps

cacheGet :: Text -> Text -> Map Text Text -> IO (Maybe ByteString)
cacheGet dbPath url query =
    runSqlite dbPath $ do
        result <- DP.selectFirst [CacheKey ==. mkCacheKey url query] []
        pure $ (\(Entity _ val) -> cacheResponse val) <$> result

cacheSet :: Text -> Text -> Map Text Text -> ByteString -> IO ()
cacheSet dbPath url query response =
    runSqlite dbPath $ do
        let key = mkCacheKey url query
        DP.deleteWhere [CacheKey ==. key]
        void $ DP.insert (Cache key response)

cachedGetRequest :: Text -> Text -> Bool -> Map Text Text -> IO (Int, ByteString)
cachedGetRequest dbPath url limitReached queryParams = do
    cachedResult <- cacheGet dbPath url queryParams
    case cachedResult of
        Just response -> do
          pure (200, response)
        Nothing -> do
          (code, responseBody) <- if limitReached
            then pure (443, "avoiding http requests (limit reached)")
            else getRequest url queryParams
          when (code == 200) $ cacheSet dbPath url queryParams responseBody
          pure (code, responseBody)
