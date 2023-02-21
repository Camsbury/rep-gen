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
import RepGen.Score ()
import RepGen.Score.Type
import RepGen.Type
--------------------------------------------------------------------------------
import qualified Foreign.C.String as FC
import qualified RepGen.PyChess as PyC
import qualified Data.Aeson as J
import qualified Data.Aeson.Types as J
--------------------------------------------------------------------------------

-- | Convert a 'Fen' into a Vector/sequence of 'EngineCandidate'
-- FIXME: update to use the cloud cache and revert to the engine if empty
fenToEngineCandidates
  :: ( MonadReader RGConfig m
    , MonadError  RGError  m
    , MonadIO m
    )
  => Fen
  -> m [EngineCandidate]
fenToEngineCandidates (Fen fen) = do
  depth <- view $ engineConfig . engineDepth
  mCount <- view $ engineConfig . engineMoveCount
  cUcis <- liftIO . FC.newCString . unpack $ fen
  cResult <- liftIO $ PyC.fen_to_engine_candidates cUcis depth mCount
  jsonString <- liftIO $ FC.peekCString cResult
  color <- view colorL
  eMoves
    <- throwEither
    . first pack
    . J.eitherDecode
    $ fromString jsonString
  extractFilteredMoves $ applyScoreColor color <$> eMoves

-- | Fetch the engine score for a given FEN
fenToScore
  :: ( MonadReader RGConfig m
    , MonadError  RGError  m
    , MonadIO m
    )
  => Fen
  -> m (Maybe Score)
fenToScore fen = do
  cands <- fenToEngineCandidates fen
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
  let sorted = sortBy (compare `on` view ngnScore) cands
  aLoss <- view $ engineConfig . engineAllowableLoss
  bestScore
    <- throwMaybe "No engine candidates to filter!?"
    $ sorted ^? ix 0 . ngnScore . scoreL
  pure $ filter (\x -> aLoss < x ^. ngnScore . scoreL / bestScore) sorted
