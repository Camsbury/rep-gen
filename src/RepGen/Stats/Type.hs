{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
--------------------------------------------------------------------------------
module RepGen.Stats.Type where
--------------------------------------------------------------------------------
import RepGen.Type
--------------------------------------------------------------------------------

-- | Provide a nominal and aggregate statistic
data RGStat
  = RGStat
  { _nom :: Double
  , _agg :: Double
  } deriving (Show, Eq)
makeLenses ''RGStat

-- | Convenience function for a initializing a stat
mkRGStat :: Double -> RGStat
mkRGStat d = RGStat d d

-- | Node stats per source (lichess, masters)
data NodeStats
  = NodeStats
  { _whiteWins :: RGStat
  , _blackWins :: RGStat
  , _prob      :: Double
  , _playCount :: Int
  } deriving (Show, Eq)
makeLenses ''NodeStats

myWins :: Color -> Lens' NodeStats RGStat
myWins White = whiteWins
myWins Black = blackWins

oppWins :: Color -> Lens' NodeStats RGStat
oppWins White = blackWins
oppWins Black = whiteWins

type ProbPrune = Double
type ProbAgg   = Double

data RGStats
  = RGStats
  { _lichessStats :: Maybe NodeStats
  , _mastersStats :: Maybe NodeStats
  , _rgScore      :: Maybe RGStat
  , _probPrune    :: ProbPrune
  , _probAgg      :: ProbAgg
  } deriving (Show, Eq)
makeLenses ''RGStats

-- | Make an RGStats given a 'ProbPrune' and a 'ProbAgg'
mkRGStats :: ProbPrune -> ProbAgg -> RGStats
mkRGStats = RGStats Nothing Nothing Nothing

instance Default RGStats where
  def = RGStats Nothing Nothing Nothing 1 1
