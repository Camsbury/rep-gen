{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
module RepGen.MoveTree.Type where

import Prelude

import Control.Lens (makeLenses)
import RepGen.Type

data WinStats
  = WinStats
  { _wins     :: Maybe Double
  , _winsAgg  :: Maybe Double
  } deriving (Show, Eq)
makeLenses ''WinStats

instance Default WinStats where
  def = WinStats
      { _wins     = Nothing
      , _winsAgg  = Nothing
      }

-- | Node stats per source (lichess, masters)
data NodeStats
  = NodeStats
  { _whiteStats :: WinStats
  , _blackStats :: WinStats
  , _prob       :: Maybe Double
  } deriving (Show, Eq)
makeLenses ''NodeStats

instance Default NodeStats where
  def = NodeStats
      { _whiteStats = def
      , _blackStats = def
      , _prob       = Nothing
      }

-- | Stats that are universal to a node
data SharedStats
  = SharedStats
  { _score      :: Maybe Double
  , _scoreAgg   :: Maybe Double
  , _probAgg    :: Maybe Double
  } deriving (Show, Eq)
makeLenses ''SharedStats

instance Default SharedStats where
  def = SharedStats
      { _score    = Nothing
      , _scoreAgg = Nothing
      , _probAgg  = Nothing
      }

-- | A node in the tree of candidates and responses in the generated repertoire
data TreeNode
  = TreeNode
  { _lichessStats :: NodeStats
  , _mastersStats :: NodeStats
  , _sharedStats  :: SharedStats
  , _ucis         :: Vector Uci
  , _responses    :: Vector (Uci, TreeNode)
  } deriving (Show, Eq)
makeLenses ''TreeNode

instance Default TreeNode where
  def = TreeNode
      { _lichessStats = def
      , _mastersStats = def
      , _sharedStats  = def
      , _ucis         = empty
      , _responses    = empty
      }

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
