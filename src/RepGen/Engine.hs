{-# OPTIONS_GHC -Wno-orphans #-}
--------------------------------------------------------------------------------
module RepGen.Engine where
--------------------------------------------------------------------------------
import Data.Aeson
  ( FromJSON(..)
  , Value(..)
  , (.:)
  )
import Data.Aeson.Types (parseFail)
import RepGen.Engine.Type
import RepGen.Score ()
--------------------------------------------------------------------------------

instance FromJSON EngineCandidate where
  parseJSON (Object v) =
    EngineCandidate
      <$> v .: "uci"
      <*> v .: "score"
  parseJSON _ = parseFail "EngineCandidate not provided as a JSON object"

