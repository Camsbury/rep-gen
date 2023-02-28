{-# LANGUAGE TemplateHaskell #-}
--------------------------------------------------------------------------------
module RepGen.Config.Type where
--------------------------------------------------------------------------------
import RepGen.Lichess.History.Type
import RepGen.Type
import RepGen.Strategy.Type
--------------------------------------------------------------------------------

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
  { _asymCandBreadth :: Int
  , _asymRespProb    :: Double
  , _colorL          :: Color
  , _engineCachePath :: Text
  , _exportJSONPath  :: Text
  , _exportP         :: Bool
  , _exportPgnPath   :: Text
  , _historyConfig   :: HistoryConfig
  , _httpCachePath   :: Text
  , _initCandBreadth :: Int
  , _initRespProb    :: Double
  , _mOverrides      :: Map [San] San
  , _mastersP        :: Bool
  , _minLogLevel     :: LogLevel
  , _minPlays        :: Int
  , _minProbAgg      :: Double
  , _minTotalMasters :: Int
  , _overridesL      :: Map Fen Uci
  , _searchDepth     :: Int
  , _startingMoves   :: [San]
  , _strategy        :: RGStrategy
  } deriving (Show, Eq)
makeLenses ''RGConfig

instance Default RGConfig where
  def = RGConfig
      { _colorL          = White
      , _engineCachePath = "./resources/engine-cache.db"
      , _exportJSONPath  = "./resources/move-tree.json"
      , _exportP         = True
      , _exportPgnPath   = "./resources/move-tree.pgn"
      , _historyConfig   = def
      , _httpCachePath   = "./resources/http-cache.db"
      , _initCandBreadth = 25
      , _asymCandBreadth = 5 -- need to enforce this is less
      , _initRespProb    = 0.001
      , _asymRespProb    = 0.02
      , _mOverrides      = mempty
      , _mastersP        = True
      , _minLogLevel     = LevelInfo
      , _minPlays        = 100
      , _minProbAgg      = 0.0003
      , _minTotalMasters = 500
      , _overridesL      = mempty
      -- this seems useless in the grand scheme, but useful in shorter analysis
      , _searchDepth     = 15
      , _startingMoves   = []
      , _strategy        = def
      }
