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
  -- | The count of candidates to check
  :: Maybe Int
  -> Fen
  -> RGM [EngineCandidate]
fenToEngineCandidates mCountMay fen = do
  useEngine <- view $ strategy . satisficers . engineFilter . engineP
  if useEngine
    then do
      limitReached <- use cloudLimitReached
      (mCands, limitReached') <- (`runStateT` limitReached) $ do
        deepCands <- fenToCloudCandidates lcDeepBreadth fen
        wideCands <- if isJust deepCands
          then fenToCloudCandidates
            (max (fromMaybe lcWideBreadth mCountMay) lcDeepBreadth)
            fen
          else pure Nothing
        pure $ maybe deepCands (\x -> mergeCands x <$> deepCands) wideCands
      when limitReached' $ cloudLimitReached .= True
      pModule <- use chessHelpers
      localCands <- L.fenToLocalCandidates pModule mCountMay fen
      let eMoves = maybe localCands (mergeCands localCands) mCands
      color <- view colorL
      pure $ applyScoreColor color <$> eMoves
    else pure []

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
  => Int
  -> Fen
  -> m (Maybe [EngineCandidate])
fenToCloudCandidates breadth (Fen fen) = do
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
  -> Maybe Int
  -> Fen
  -> m [EngineCandidate]
fenToEngineCandidatesInit pModule mCountMay fen = do
  useEngine <- view $ strategy . satisficers . engineFilter . engineP
  if useEngine
    then do
      mCands <- (`evalStateT` False) $ do
        deepCands <- fenToCloudCandidates lcDeepBreadth fen
        wideCands <- if isJust deepCands
          then fenToCloudCandidates lcWideBreadth fen
          else pure Nothing
        pure $ maybe deepCands (\x -> mergeCands x <$> deepCands) wideCands
      eMoves <- maybe (L.fenToLocalCandidates pModule mCountMay fen) pure mCands
      color <- view colorL
      pure $ applyScoreColor color <$> eMoves
    else pure []

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
  useEngine <- view $ strategy . satisficers . engineFilter . engineP
  if useEngine
    then do
      color <- view colorL
      cands <- fenToEngineCandidatesInit pModule Nothing fen
      case color of
        White -> pure $ cands ^? ix 0 . ngnScore
        Black -> pure $ cands ^? ix 0 . ngnScore . to (\x -> x & scoreL %~ (1 -))
    else pure Nothing

applyScoreColor :: Color -> EngineCandidate -> EngineCandidate
applyScoreColor White = id
applyScoreColor Black = ngnScore . scoreL %~ (1 -)

injectEngine :: [EngineCandidate] -> Maybe Double -> (Uci, (Fen, PosInfo)) -> (Uci, (Fen, PosInfo))
injectEngine nCands bestMay info@(uci, _)
  = if isJust bestMay
  then scored
  else scoredCopied
  where
    findBy _ [] = Nothing
    findBy u (ngn:rest)
      | u == ngn ^. ngnUci = Just $ ngn ^. ngnScore . scoreL
      | otherwise = findBy u rest
    scored
      = info
      & _2 . _2 . posStats . rgScore    .~ (mkRGStat <$> findBy uci nCands)
      & _2 . _2 . posStats . bestScoreL .~ bestMay
    scoredCopied
      = scored & _2 . _2 . posStats . bestScoreL .~ findBy uci nCands
