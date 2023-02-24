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
  { _httpCachePath   :: Text
  , _colorL          :: Color
  , _exportP         :: Bool
  , _exportPgnPath   :: Text
  , _exportJSONPath  :: Text
  , _historyConfig   :: HistoryConfig
  , _mastersP        :: Bool
  , _initCandBreadth :: Int
  , _asymCandBreadth :: Int
  , _minLogLevel     :: LogLevel
  , _minPlays        :: Int
  , _minProbAgg      :: Double
  , _initRespProb    :: Double
  , _asymRespProb    :: Double
  , _minTotalMasters :: Int
  , _startingMoves   :: [San]
  , _mOverrides      :: Map [San] San
  , _overridesL      :: Map Fen Uci
  , _searchDepth     :: Int
  , _strategy        :: RGStrategy
  } deriving (Show, Eq)
makeLenses ''RGConfig

instance Default RGConfig where
  def = RGConfig
      { _httpCachePath       = "./resources/http-cache.db"
      , _colorL          = White
      , _exportP         = True
      , _exportPgnPath   = "./resources/move-tree.pgn"
      , _exportJSONPath  = "./resources/move-tree.json"
      , _historyConfig   = def
      , _mastersP        = True
      , _initCandBreadth = 25
      , _asymCandBreadth = 5
      , _minLogLevel     = LevelInfo
      , _minPlays        = 100
      , _minProbAgg      = 0.0003
      , _initRespProb    = 0.01
      , _asymRespProb    = 0.25
      , _minTotalMasters = 500
      , _startingMoves   = []
      , _mOverrides      = mempty
      , _overridesL      = mempty
      , _searchDepth     = 6
      , _strategy        = def
      }
