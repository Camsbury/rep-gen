{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
module RepGen.MoveTree.Type where

import Prelude

import Control.Lens (makeLenses)
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

-- | A node in the tree of candidates and responses in the generated repertoire
data TreeNode
  = TreeNode
  { _rgStats   :: RGStats
  , _ucis      :: Vector Uci
  , _responses :: Vector (Uci, TreeNode)
  } deriving (Show, Eq)
makeLenses ''TreeNode

instance Default TreeNode where
  def = TreeNode def empty empty

-- | Provide a traversal into the move tree for a given list of ucis
traverseUcis :: Vector Uci -> Traversal' TreeNode TreeNode
traverseUcis = foldl' f $ prism' id Just
  where
    f p uci
      = p
      . responses
      . traversed
      . filtered (\(x, y) -> x == uci)
      . _2
