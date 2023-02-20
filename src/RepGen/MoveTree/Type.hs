{-# LANGUAGE TemplateHaskell #-}
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
  , _nodeFen   :: Fen
  , _responses :: Vector (Uci, TreeNode)
  , _removed   :: Bool
  } deriving (Show, Eq)
makeLenses ''TreeNode

instance Default TreeNode where
  def = TreeNode def empty def empty False
