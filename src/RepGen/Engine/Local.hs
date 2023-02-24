{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveAnyClass #-}
--------------------------------------------------------------------------------
module RepGen.Engine.Local
  ( fenToLocalCandidates
  , migrateAll
  ) where
--------------------------------------------------------------------------------
import RepGen.Config.Type
import RepGen.Engine.Type
import RepGen.Strategy.Type
import RepGen.Type
--------------------------------------------------------------------------------
import Database.Persist (Entity(..), (==.))
import Database.Persist.TH (mkPersist, mkMigrate, persistLowerCase, share, sqlSettings)
import Database.Persist.Sqlite (runSqlite)
--------------------------------------------------------------------------------
import qualified Foreign.C.String as FC
import qualified RepGen.PyChess as PyC
import qualified Data.Aeson as J
import qualified Database.Persist as DP
--------------------------------------------------------------------------------

doFenToLocalCandidates
  :: ( MonadReader RGConfig m
    , MonadError  RGError  m
    , MonadIO m
    )
  => Text
  -> Int
  -> Int
  -> m ByteString
doFenToLocalCandidates fen depth mCount = do
  cFen <- liftIO . FC.newCString . unpack $ fen
  cResult <- liftIO $ PyC.fen_to_engine_candidates cFen depth mCount
  fmap fromString . liftIO $ FC.peekCString cResult

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Cache
    key Text
    response ByteString
    deriving Eq Show
|]

mkCacheKey :: Text -> Int -> Int -> Text
mkCacheKey fen depth moveCount
  = fen <> "|depth " <> tshow depth <> "|moveCount " <> tshow moveCount <> "|"

cacheGet :: Text -> Text -> Int -> Int -> IO (Maybe ByteString)
cacheGet dbPath fen depth moveCount =
    runSqlite dbPath $ do
        result <- DP.selectFirst [CacheKey ==. mkCacheKey fen depth moveCount] []
        pure $ (\(Entity _ val) -> cacheResponse val) <$> result

cacheSet :: Text -> Text -> Int -> Int -> ByteString -> IO ()
cacheSet dbPath fen depth moveCount response =
    runSqlite dbPath $ do
        let key = mkCacheKey fen depth moveCount
        DP.deleteWhere [CacheKey ==. key]
        void $ DP.insert (Cache key response)

fenToLocalCandidates
  :: ( MonadReader RGConfig m
    , MonadError  RGError  m
    , MonadIO m
    )
  => Fen
  -> m [EngineCandidate]
fenToLocalCandidates (Fen fen) = do
  depth
    <- view
    $ strategy
    . satisficers
    . engineFilter
    . engineDepth
  mCount
    <- view
    $ strategy
    . satisficers
    . engineFilter
    . engineMoveCount
  dbPath <- view engineCachePath
  cachedResult <- liftIO $ cacheGet dbPath fen depth mCount
  jsonString <- case cachedResult of
    Just res -> pure res
    Nothing -> do
      res <- doFenToLocalCandidates fen depth mCount
      liftIO $ cacheSet dbPath fen depth mCount res
      pure res
  throwEither
    . first pack
    . J.eitherDecode
    . fromStrict
    $ jsonString
