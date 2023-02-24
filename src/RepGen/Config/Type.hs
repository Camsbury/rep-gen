{-# LANGUAGE TemplateHaskell #-}
--------------------------------------------------------------------------------
module RepGen.Config.Type where
--------------------------------------------------------------------------------
import RepGen.Lichess.History.Type
import RepGen.Type
import RepGen.Strategy.Type
--------------------------------------------------------------------------------

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
      , _historySpeeds  = [Blitz, Rapid]
      , _historyMoveCount = 30
      }

data RGConfig
  = RGConfig
  { _cachePath       :: Text
  , _colorL          :: Color
  , _engineP         :: Bool
  , _engineConfig    :: EngineConfig
  , _exportP         :: Bool
  , _exportPgnPath   :: Text
  , _exportJSONPath  :: Text
  , _historyConfig   :: HistoryConfig
  , _mastersP        :: Bool
  , _initCandBreadth :: Int
  , _asymCandBreadth :: Int
  , _minPlays        :: Int
  , _minProbAgg      :: Double
  , _initRespProb    :: Double
  , _asymRespProb    :: Double
  , _minTotalMasters :: Int
  , _startingMoves   :: Vector San
  , _overridesL      :: Map Fen Uci
  , _searchDepth     :: Int
  , _strategy        :: RGStrategy
  } deriving (Show, Eq)
makeLenses ''RGConfig

instance Default RGConfig where
  def = RGConfig
      { _cachePath       = "./resources/cache.db"
      , _colorL          = White
      , _engineP         = True
      , _engineConfig    = def
      , _exportP         = True
      , _exportPgnPath   = "./resources/move-tree.pgn"
      , _exportJSONPath  = "./resources/move-tree.json"
      , _historyConfig   = def
      , _mastersP        = True
      , _initCandBreadth = 25
      , _asymCandBreadth = 5
      , _minPlays        = 100
      , _minProbAgg      = 0.0003
      , _initRespProb    = 0.01
      , _asymRespProb    = 0.25
      , _minTotalMasters = 500
      , _startingMoves   = []
      , _overridesL      = mempty
      , _searchDepth     = 6
      , _strategy        = MaxWinOverLoss
      }
