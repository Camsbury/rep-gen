{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
--------------------------------------------------------------------------------
module RepGen.MoveTree.Type where
--------------------------------------------------------------------------------
import RepGen.Type
import RepGen.Stats.Type
--------------------------------------------------------------------------------

-- | A node in the tree of candidates and responses in the generated repertoire
data TreeNode
  = TreeNode
  { _rgStats   :: RGStats
  , _uciPath   :: Vector Uci
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
      . filtered (\(x, _) -> x == uci)
      . _2
