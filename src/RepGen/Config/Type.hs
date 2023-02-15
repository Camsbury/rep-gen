{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}
module RepGen.Config.Type where

import Prelude

import Control.Lens (makeLenses)
import Control.Lens.Operators
import Data.Default
import RepGen.Lichess.History.Type
import RepGen.Type

data EngineConfig
  = EngineConfig
  { _engineAllowableLoss :: Double
  , _engineMoveCount :: Int
  , _engineDepth :: Int
  , _engineHash :: Int
  , _engineThreads :: Int
  } deriving (Show, Eq)
makeLenses ''EngineConfig

instance Default EngineConfig where
  def = EngineConfig
      { _engineAllowableLoss = 0.9
      , _engineMoveCount = 10
      , _engineDepth = 20
      , _engineHash = 2048
      , _engineThreads = 7
      }

data HistoryConfig
  = HistoryConfig
  { _historyRatings   :: Vector LichessRating
  , _historySpeeds    :: Vector GameSpeed
  , _historyMoveCount :: Int
  } deriving (Show, Eq)
makeLenses ''HistoryConfig

instance Default HistoryConfig where
  def = HistoryConfig
      { _historyRatings = [L2000, L2200, L2500]
      , _historySpeeds  = [Bullet, Blitz, Rapid]
      , _historyMoveCount = 30
      }

data RGConfig
  = RGConfig
  { _engineP         :: Bool
  , _engineConfig    :: EngineConfig
  , _exportP         :: Bool
  , _exportPgnPath   :: Maybe Text
  , _exportTreePath  :: Maybe Text
  , _historyConfig   :: HistoryConfig
  , _mastersP        :: Bool
  , _maxCandBreadth  :: Int
  , _minCandProb     :: Double
  , _minPlays        :: Int
  , _minProbAgg      :: Double
  , _minRespProb     :: Double
  , _minTotalMasters :: Int
  , _moves           :: Vector San
  , _searchDepth     :: Int
  , _strategy        :: RGStrategy
  } deriving (Show, Eq)
makeLenses ''RGConfig

instance Default RGConfig where
  def = RGConfig
      { _engineP         = True
      , _engineConfig    = def
      , _exportP         = True
      , _exportPgnPath   = Nothing
      , _exportTreePath  = Nothing
      , _historyConfig   = def
      , _mastersP        = True
      , _maxCandBreadth  = 5
      , _minCandProb     = 0.01
      , _minPlays        = 100
      , _minProbAgg      = 0.01
      , _minRespProb     = 0.05
      , _minTotalMasters = 500
      , _moves           = []
      , _searchDepth     = 5
      , _strategy        = MaxWinOverLoss
      }
