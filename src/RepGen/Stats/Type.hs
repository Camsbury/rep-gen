{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
module RepGen.Stats.Type where

import RepGen.Type

-- | Provide a nominal and aggregate statistic
data RGStat
  = RGStat
  { _nom :: Double
  , _agg :: Double
  } deriving (Show, Eq)
makeLenses ''RGStat

-- | Node stats per source (lichess, masters)
data NodeStats
  = NodeStats
  { _whiteWins :: RGStat
  , _blackWins :: RGStat
  , _prob      :: Double
  } deriving (Show, Eq)
makeLenses ''NodeStats

myWins :: Color -> Lens' NodeStats RGStat
myWins White = whiteWins
myWins Black = blackWins

oppWins :: Color -> Lens' NodeStats RGStat
oppWins White = blackWins
oppWins Black = whiteWins

data RGStats
  = RGStats
  { _lichessStats :: Maybe NodeStats
  , _mastersStats :: Maybe NodeStats
  , _score        :: Maybe RGStat
  , _probAgg      :: Double
  } deriving (Show, Eq)
makeLenses ''RGStats

instance Default RGStats where
  def = RGStats Nothing Nothing Nothing 1
