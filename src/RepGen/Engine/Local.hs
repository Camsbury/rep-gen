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
import RepGen.PyChess.Type
import RepGen.Strategy.Type
import RepGen.Type
--------------------------------------------------------------------------------
import Database.Persist (Entity(..), (==.))
import Database.Persist.TH (mkPersist, mkMigrate, persistLowerCase, share, sqlSettings)
import Database.Persist.Sqlite (runSqlite)
import Foreign.Ptr
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
  => Ptr PyObject
  -> Text
  -> Int
  -> Int
  -> m ByteString
doFenToLocalCandidates pModule fen depth mCount = do
  cFen <- liftIO . FC.newCString . unpack $ fen
  cResult <- liftIO $ PyC.fen_to_engine_candidates pModule cFen depth mCount
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
  => Ptr PyObject
  -> Maybe Int
  -> Fen
  -> m [EngineCandidate]
fenToLocalCandidates pModule mCountMay (Fen fen) = do
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
  cachedResult <- liftIO $ cacheGet dbPath fen depth (fromMaybe mCount mCountMay)
  jsonString <- case cachedResult of
    Just res -> pure res
    Nothing -> do
      res <- doFenToLocalCandidates pModule fen depth mCount
      liftIO $ cacheSet dbPath fen depth mCount res
      pure res
  throwEither
    . first pack
    . J.eitherDecode
    . fromStrict
    $ jsonString
