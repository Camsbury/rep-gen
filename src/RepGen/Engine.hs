{-# OPTIONS_GHC -Wno-orphans #-}
--------------------------------------------------------------------------------
module RepGen.Engine where
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
import RepGen.Type
--------------------------------------------------------------------------------
import qualified Foreign.C.String as FC
import qualified RepGen.PyChess as PyC
import qualified Data.Aeson as J
import qualified Data.Aeson.Types as J
--------------------------------------------------------------------------------

-- | Convert a 'Fen' into a Vector/sequence of 'EngineCandidate'
fenToEngineCandidates
  :: Fen
  -> Int
  -> Int
  -> RGM (Vector EngineCandidate)
fenToEngineCandidates (Fen fen) depth mCount = do
  cUcis <- liftIO . FC.newCString . unpack $ fen
  cResult <- liftIO $ PyC.fen_to_engine_candidates cUcis depth mCount
  jsonString <- liftIO $ FC.peekCString cResult
  color <- view colorL
  fmap (fmap (applyScoreColor color))
    . throwEither
    . first pack
    . J.eitherDecode
    $ fromString jsonString

instance FromJSON EngineCandidate where
  parseJSON (Object v) =
    EngineCandidate
      <$> v .: "uci"
      <*> v .: "score"
  parseJSON _ = J.parseFail "EngineCandidate not provided as a JSON object"

applyScoreColor :: Color -> EngineCandidate -> EngineCandidate
applyScoreColor White = id
applyScoreColor Black = ngnScore . scoreL %~ negate
