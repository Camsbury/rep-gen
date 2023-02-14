{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}
module RepGen.Config.Type where

import Prelude
import RepGen.Type
import RepGen.Lichess.History.Type
import Control.Lens (makeLenses, makeLensesFor)
import Data.Default

data EngineConfig
  = EngineConfig
  { _allowableLoss :: Double
  , _moveCount :: Int
  , _depth :: Int
  , _hash :: Int
  , _threads :: Int
  } deriving (Show, Eq)
makeLensesFor [("_", "engine")] ''EngineConfig

data HistoryConfig
  = HistoryConfig
  { _ratings   :: Vector LichessRating
  , _speeds    :: Vector GameSpeed
  , _moveCount :: Int
  } deriving (Show, Eq)
makeLensesFor [("_", "history")] ''HistoryConfig

data RGConfig
  = RGConfig
  { _exportP :: Bool
  , _minProbAgg :: Double
  , _minRespProb :: Double
  , _minCandProb :: Double
  , _minPlays :: Int
  , _minTotalMasters :: Int
  , _maxCandBreadth :: Int
  , _searchDepth :: Int
  , _strategy :: RGStrategy
  , _engineConfig :: EngineConfig
  } deriving (Show, Eq)
makeLenses ''RGConfig

instance Default RGConfig where
  def = undefined
