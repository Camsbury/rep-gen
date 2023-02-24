{-# OPTIONS_GHC -Wno-orphans #-}
--------------------------------------------------------------------------------
module RepGen.Engine
  ( fenToEngineCandidates
  , fenToScore
  ) where
--------------------------------------------------------------------------------
import RepGen.Config.Type
import RepGen.Engine.Type
import RepGen.Monad
import RepGen.Score ()
import RepGen.Score.Type
import RepGen.State.Type
import RepGen.Strategy.Type
import RepGen.Type
--------------------------------------------------------------------------------
import qualified RepGen.Engine.Local as L
import qualified Data.Aeson as J
import qualified Web
--------------------------------------------------------------------------------

cacheUrl :: Text
cacheUrl = "https://lichess.org/api/cloud-eval"

lcDeepBreadth :: Int
lcDeepBreadth = 5

-- TODO: ask LC what is going on with their max
lcWideBreadth :: Int
lcWideBreadth = 10

-- NOTE: currently just stops at 5 if there are no "wide" candidates,
-- so not as many options
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
  eMoves <- maybe (L.fenToLocalCandidates fen) pure mCands
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
  dbPath <- view httpCachePath
  (statusCode, response)
    <- liftIO
    . Web.cachedGetRequest dbPath cacheUrl
    $ mapFromList [("fen", fen), ("multiPv", tshow breadth)]
  case statusCode of
    200 -> throwEither
        . bimap pack (pure . view cloudCands)
        . J.eitherDecode
        . fromStrict
        $ response
    404 -> do
      logDebugN
        $ "FEN missing from cache: "
        <> tshow fen
      pure Nothing
    code -> do
      logWarnN
        $ "HTTP error code: "
        <> tshow code
        <> " for FEN: "
        <> tshow fen
      logWarnN "Stopping cloud eval usage to avoid getting banned"
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
  eMoves <- maybe (L.fenToLocalCandidates fen) pure mCands
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

applyScoreColor :: Color -> EngineCandidate -> EngineCandidate
applyScoreColor White = id
applyScoreColor Black = ngnScore . scoreL %~ (1 -)


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
  aLoss
    <- view
    $ strategy
    . satisficers
    . engineFilter
    . engineAllowableLoss
  bestScore
    <- throwMaybe "No engine candidates to filter!?"
    $ sorted ^? ix 0 . ngnScore . scoreL
  pure $ filter (\x -> aLoss < x ^. ngnScore . scoreL / bestScore) sorted
