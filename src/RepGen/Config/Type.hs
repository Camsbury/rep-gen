{-# LANGUAGE TemplateHaskell #-}
--------------------------------------------------------------------------------
module RepGen.Config.Type where
--------------------------------------------------------------------------------
import RepGen.Lichess.History.Type
import RepGen.Type
import RepGen.Strategy.Type
--------------------------------------------------------------------------------
import Data.Aeson
  ( FromJSON(..)
  , (.:?)
  )
--------------------------------------------------------------------------------
import qualified Data.Aeson as J
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

instance FromJSON HistoryConfig where
  parseJSON
    = J.withObject "HistoryConfig" $ \o -> do
      let updateIfPresent fName f c = maybe c (\a -> c & f .~ a) <$> o .:? fName
      updateIfPresent "historyRatings" historyRatings def
        >>= updateIfPresent "historySpeeds" historySpeeds
        >>= updateIfPresent "historyMoveCount"  historyMoveCount

data RGConfig
  = RGConfig
  { _asymCandBreadth :: Int
  , _asymRespProb    :: Double
  , _colorL          :: Color
  , _engineCachePath :: Text
  , _exportTreePath  :: Text
  , _exportInfoPath  :: Text
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
      , _exportTreePath  = "./resources/move-tree.json"
      , _exportInfoPath  = "./resources/pos-info.json"
      , _exportP         = True
      , _exportPgnPath   = "./resources/move-tree.pgn"
      , _historyConfig   = def
      , _httpCachePath   = "./resources/http-cache.db"
      , _initCandBreadth = 10
      , _asymCandBreadth = 5 -- need to enforce this is less
      , _initRespProb    = 0.002 -- want to capture all the main first moves
      , _asymRespProb    = 0.25 -- want to provide enough breadth for decision making later on
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

instance FromJSON RGConfig where
  parseJSON
    = J.withObject "RGConfig" $ \o -> do
      let updateIfPresent fName f c = maybe c (\a -> c & f .~ a) <$> o .:? fName
      updateIfPresent "colorL" colorL def
        >>= updateIfPresent "engineCachePath" engineCachePath
        >>= updateIfPresent "exportTreePath"  exportTreePath
        >>= updateIfPresent "exportInfoPath"  exportInfoPath
        >>= updateIfPresent "exportP"         exportP
        >>= updateIfPresent "exportPgnPath"   exportPgnPath
        >>= updateIfPresent "historyConfig"   historyConfig
        >>= updateIfPresent "httpCachePath"   httpCachePath
        >>= updateIfPresent "initCandBreadth" initCandBreadth
        >>= updateIfPresent "asymCandBreadth" asymCandBreadth
        >>= updateIfPresent "initRespProb"    initRespProb
        >>= updateIfPresent "asymRespProb"    asymRespProb
        >>= updateIfPresent "mOverrides"      mOverrides
        >>= updateIfPresent "mastersP"        mastersP
        >>= updateIfPresent "minLogLevel"     minLogLevel
        >>= updateIfPresent "minPlays"        minPlays
        >>= updateIfPresent "minProbAgg"      minProbAgg
        >>= updateIfPresent "minTotalMasters" minTotalMasters
        >>= updateIfPresent "overridesL"      overridesL
        >>= updateIfPresent "searchDepth"     searchDepth
        >>= updateIfPresent "startingMoves"   startingMoves
        >>= updateIfPresent "strategy"        strategy
