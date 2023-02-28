{-# OPTIONS_GHC -Wno-orphans #-}
--------------------------------------------------------------------------------
module RepGen.Engine
  ( fenToEngineCandidates
  , fenToScore
  , injectEngine
  ) where
--------------------------------------------------------------------------------
import Foreign.Ptr
import RepGen.Config.Type
import RepGen.Engine.Type
import RepGen.Monad
import RepGen.PyChess.Type
import RepGen.Score ()
import RepGen.Score.Type
import RepGen.State.Type
import RepGen.Stats.Type
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
  limitReached <- use cloudLimitReached
  (mCands, limitReached') <- (`runStateT` limitReached) $ do
    deepCands <- fenToCloudCandidates fen lcDeepBreadth
    wideCands <- if isJust deepCands
      then fenToCloudCandidates fen lcWideBreadth
      else pure Nothing
    pure $ maybe deepCands (\x -> mergeCands x <$> deepCands) wideCands
  when limitReached' $ cloudLimitReached .= True
  pModule <- use chessHelpers
  eMoves <- maybe (L.fenToLocalCandidates pModule fen) pure mCands
  color <- view colorL
  pure $ applyScoreColor color <$> eMoves

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
  limitReached <- get
  (statusCode, response)
    <- liftIO
    . Web.cachedGetRequest dbPath cacheUrl limitReached
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
    -- the case where we are only using the cache
    0 -> do
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
  => Ptr PyObject
  -> Fen
  -> m [EngineCandidate]
fenToEngineCandidatesInit pModule fen = do
  mCands <- (`evalStateT` False) $ do
    deepCands <- fenToCloudCandidates fen lcDeepBreadth
    wideCands <- if isJust deepCands
      then fenToCloudCandidates fen lcWideBreadth
      else pure Nothing
    pure $ maybe deepCands (\x -> mergeCands x <$> deepCands) wideCands
  eMoves <- maybe (L.fenToLocalCandidates pModule fen) pure mCands
  color <- view colorL
  pure $ applyScoreColor color <$> eMoves

-- | Fetch the engine score for a given FEN
fenToScore
  :: ( MonadReader RGConfig m
    , MonadLogger m
    , MonadError  RGError  m
    , MonadIO m
    )
  => Ptr PyObject
  -> Fen
  -> m (Maybe Score)
fenToScore pModule fen = do
  color <- view colorL
  cands <- fenToEngineCandidatesInit pModule fen
  case color of
    White -> pure $ cands ^? ix 0 . ngnScore
    Black -> pure $ cands ^? ix 0 . ngnScore . to (\x -> x & scoreL %~ (1 -))

applyScoreColor :: Color -> EngineCandidate -> EngineCandidate
applyScoreColor White = id
applyScoreColor Black = ngnScore . scoreL %~ (1 -)

injectEngine :: [EngineCandidate] -> (Uci, (Fen, PosInfo)) -> (Uci, (Fen, PosInfo))
injectEngine nCands info@(uci, _)
  = info
  & _2
  . _2
  . posStats
  . rgScore
  .~ findBy uci nCands
  where
    findBy _ [] = Nothing
    findBy u (ngn:rest)
      | u == ngn ^. ngnUci = Just . mkRGStat $ ngn ^. ngnScore . scoreL
      | otherwise = findBy u rest
