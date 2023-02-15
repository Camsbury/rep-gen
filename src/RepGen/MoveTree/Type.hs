module RepGen.MoveTree.Type where

import Prelude

import RepGen.Type

data WinStats
  = WinStats
  { _wins     :: Maybe Double
  , _winsAgg  :: Maybe Double
  , _winsM    :: Maybe Double
  , _winsMAgg :: Maybe Double
  } deriving (Show, Eq)

instance Default WinStats where
  def = WinStats
      { _wins     = Nothing
      , _winsAgg  = Nothing
      , _winsM    = Nothing
      , _winsMAgg = Nothing
      }

-- | A node in the tree of candidates and responses in the generated repertoire
data TreeNode
  = TreeNode
  { _whiteStats :: WinStats
  , _blackStats :: WinStats
  , _score      :: Maybe Double
  , _scoreAgg   :: Maybe Double
  , _prob       :: Maybe Double
  , _probM      :: Maybe Double
  , _probAgg    :: Maybe Double
  , _ucis       :: Vector Uci
  , _responses  :: Vector (Uci, TreeNode)
  } deriving (Show, Eq)

instance Default TreeNode where
  def = TreeNode
      { _whiteStats = def
      , _blackStats = def
      , _score      = Nothing
      , _scoreAgg   = Nothing
      , _prob       = Nothing
      , _probM      = Nothing
      , _probAgg    = Nothing
      , _ucis       = empty
      , _responses  = empty
      }
