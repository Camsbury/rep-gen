{-# OPTIONS_GHC -Wno-orphans #-}
--------------------------------------------------------------------------------
module RepGen.Engine
  ( fenToEngineCandidates
  , fenToScore
  ) where
--------------------------------------------------------------------------------
import Data.Aeson
  ( FromJSON(..)
  , Value(..)
  , (.:)
  )
import RepGen.Config.Type
import RepGen.Engine.Type
import RepGen.Monad
import RepGen.Score ()
import RepGen.Score.Type
import RepGen.State.Type
import RepGen.Type
--------------------------------------------------------------------------------
import qualified Foreign.C.String as FC
import qualified RepGen.PyChess as PyC
import qualified Data.Aeson as J
import qualified Data.Aeson.Types as J
import qualified Web
--------------------------------------------------------------------------------

cacheUrl :: Text
cacheUrl = "https://lichess.org/api/cloud-eval"

lcDeepBreadth :: Int
lcDeepBreadth = 5

-- TODO: ask LC what is going on with their max
lcWideBreadth :: Int
lcWideBreadth = 10

fenToEngineCandidates
  :: Fen
  -> RGM [EngineCandidate]
fenToEngineCandidates fen = do
  (mCands, limitReached) <- (`runStateT` False) $ do
    deepCands <- fenToCloudCandidates fen lcDeepBreadth
    wideCands <- if isJust deepCands
      then fenToCloudCandidates fen lcWideBreadth
      else pure Nothing
    pure $ maybe deepCands (\x -> mergeCands x <$> deepCands) wideCands
  when limitReached $ cloudLimitReached .= True
  eMoves <- maybe (fenToLocalCandidates fen) pure mCands
  color <- view colorL
  extractFilteredMoves $ applyScoreColor color <$> eMoves

mergeCands
  :: [EngineCandidate]
  -> [EngineCandidate]
  -> [EngineCandidate]
mergeCands wideCands deepCands
  = ((wideCands ^. engineToMap)
     `union`
     (deepCands ^. engineToMap)) ^. from engineToMap

-- TODO: Cache these too!!
fenToLocalCandidates
  :: ( MonadReader RGConfig m
    , MonadError  RGError  m
    , MonadIO m
    )
  => Fen
  -> m [EngineCandidate]
fenToLocalCandidates (Fen fen) = do
  depth <- view $ engineConfig . engineDepth
  mCount <- view $ engineConfig . engineMoveCount
  cUcis <- liftIO . FC.newCString . unpack $ fen
  cResult <- liftIO $ PyC.fen_to_engine_candidates cUcis depth mCount
  jsonString <- liftIO $ FC.peekCString cResult
  throwEither
    . first pack
    . J.eitherDecode
    $ fromString jsonString

-- FIXME: need to pass moveCount and maybe call twice
-- , once with 5
-- , and one with the Config move count
-- maybe have a separate move count in the config for this
fenToCloudCandidates
  :: ( MonadReader RGConfig m
    , MonadError  RGError  m
    , MonadLogger m
    , MonadIO m
    , MonadState Bool m
    )
  => Fen
  -> Int
  -> m (Maybe [EngineCandidate])
fenToCloudCandidates (Fen fen) breadth = do
  dbPath <- view cachePath
  (statusCode, response)
    <- liftIO
    . Web.cachedGetRequest dbPath cacheUrl
    $ mapFromList [("fen", fen), ("multiPv", tshow breadth)]
  case statusCode of
    200 -> throwEither
        . bimap pack (pure . view cloudCands)
        . J.eitherDecode
        . fromString
        $ unpack response
    404 -> do
      logInfoN
        $ "FEN missing from cache: "
        <> tshow fen
      pure Nothing
    code -> do
      logInfoN
        $ "HTTP error code: "
        <> tshow code
        <> " for FEN: "
        <> tshow fen
      put True
      pure Nothing

fenToEngineCandidatesInit
  :: ( MonadReader RGConfig m
    , MonadError  RGError  m
    , MonadLogger m
    , MonadIO m
    )
  => Fen
  -> m [EngineCandidate]
fenToEngineCandidatesInit fen = do
  mCands <- (`evalStateT` False) $ do
    deepCands <- fenToCloudCandidates fen lcDeepBreadth
    wideCands <- if isJust deepCands
      then fenToCloudCandidates fen lcWideBreadth
      else pure Nothing
    pure $ maybe deepCands (\x -> mergeCands x <$> deepCands) wideCands
  eMoves <- maybe (fenToLocalCandidates fen) pure mCands
  color <- view colorL
  extractFilteredMoves $ applyScoreColor color <$> eMoves

-- | Fetch the engine score for a given FEN
fenToScore
  :: ( MonadReader RGConfig m
    , MonadLogger m
    , MonadError  RGError  m
    , MonadIO m
    )
  => Fen
  -> m (Maybe Score)
fenToScore fen = do
  cands <- fenToEngineCandidatesInit fen
  pure $ cands ^? ix 0 . ngnScore

instance FromJSON EngineCandidate where
  parseJSON (Object v) =
    EngineCandidate
      <$> v .: "uci"
      <*> v .: "score"
  parseJSON _ = J.parseFail "EngineCandidate not provided as a JSON object"

applyScoreColor :: Color -> EngineCandidate -> EngineCandidate
applyScoreColor White = id
applyScoreColor Black = ngnScore . scoreL %~ negate


-- | Extract moves that are within a reasonable deviation from the best move
extractFilteredMoves
  :: ( MonadReader RGConfig m
    , MonadError  RGError  m
    , MonadIO m
    )
  => [EngineCandidate]
  -> m [EngineCandidate]
extractFilteredMoves cands = do
  let sorted = sortBy (compare `on` view (ngnScore . scoreL . to negate)) cands
  aLoss <- view $ engineConfig . engineAllowableLoss
  bestScore
    <- throwMaybe "No engine candidates to filter!?"
    $ sorted ^? ix 0 . ngnScore . scoreL
  pure $ filter (\x -> aLoss < x ^. ngnScore . scoreL / bestScore) sorted
