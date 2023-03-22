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

type TreePath = Text
type InfoPath = Text

data RGConfig
  = RGConfig
  { _asymCandBreadth :: Int
  , _asymMinPlays    :: Int
  , _asymRespProb    :: Double
  , _colorL          :: Color
  , _engineCachePath :: Text
  , _exclusionsL     :: Map Fen (Set Uci)
  , _exportTreePath  :: TreePath
  , _exportInfoPath  :: InfoPath
  , _exportP         :: Bool
  , _exportPgnPath   :: Text
  , _historyConfig   :: HistoryConfig
  , _httpCachePath   :: Text
  , _initCandBreadth :: Int
  , _initMinPlays    :: Int
  , _initRespProb    :: Double
  , _mExclusions     :: [([San], [San])]
  , _mOverrides      :: [([San], San)]
  , _mastersP        :: Bool
  , _minLogLevel     :: LogLevel
  , _minProbAgg      :: Double
  , _minTotalMasters :: Int
  , _overridesL      :: Map Fen Uci
  , _resumeFrom      :: Maybe (TreePath, InfoPath)
  , _searchDepth     :: Int
  , _startingMoves   :: [San]
  , _strategy        :: RGStrategy
  } deriving (Show, Eq)
makeLenses ''RGConfig

instance Default RGConfig where
  def = RGConfig
      { _colorL          = White
      , _asymCandBreadth = 5 -- need to enforce this is less
      , _asymMinPlays    = 100 -- minimum needed for statistical significance
      , _asymRespProb    = 0.1 -- want to provide enough breadth for decision making later on
      , _engineCachePath = "./resources/engine-cache.db"
      , _exclusionsL     = mempty
      , _exportInfoPath  = "./resources/pos-info.json"
      , _exportP         = True
      , _exportPgnPath   = "./resources/move-tree.pgn"
      , _exportTreePath  = "./resources/move-tree.json"
      , _historyConfig   = def
      , _httpCachePath   = "./resources/http-cache.db"
      , _initCandBreadth = 13
      , _initMinPlays    = 12000 -- found empirically
      , _initRespProb    = 0.025 -- want to capture all the main first moves
      , _mExclusions     = mempty
      , _mOverrides      = mempty
      , _mastersP        = True
      , _minLogLevel     = LevelInfo
      , _minProbAgg      = 0.0003
      , _minTotalMasters = 500
      , _overridesL      = mempty
      , _resumeFrom      = Nothing
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
        >>= updateIfPresent "asymCandBreadth" asymCandBreadth
        >>= updateIfPresent "asymMinPlays"    asymMinPlays
        >>= updateIfPresent "asymRespProb"    asymRespProb
        >>= updateIfPresent "engineCachePath" engineCachePath
        >>= updateIfPresent "exclusionsL"     exclusionsL
        >>= updateIfPresent "exportInfoPath"  exportInfoPath
        >>= updateIfPresent "exportP"         exportP
        >>= updateIfPresent "exportPgnPath"   exportPgnPath
        >>= updateIfPresent "exportTreePath"  exportTreePath
        >>= updateIfPresent "historyConfig"   historyConfig
        >>= updateIfPresent "httpCachePath"   httpCachePath
        >>= updateIfPresent "initCandBreadth" initCandBreadth
        >>= updateIfPresent "initMinPlays"    initMinPlays
        >>= updateIfPresent "initRespProb"    initRespProb
        >>= updateIfPresent "mExclusions"     mExclusions
        >>= updateIfPresent "mOverrides"      mOverrides
        >>= updateIfPresent "mastersP"        mastersP
        >>= updateIfPresent "minLogLevel"     minLogLevel
        >>= updateIfPresent "minProbAgg"      minProbAgg
        >>= updateIfPresent "minTotalMasters" minTotalMasters
        >>= updateIfPresent "overridesL"      overridesL
        >>= updateIfPresent "resumeFrom"      resumeFrom
        >>= updateIfPresent "searchDepth"     searchDepth
        >>= updateIfPresent "startingMoves"   startingMoves
        >>= updateIfPresent "strategy"        strategy
