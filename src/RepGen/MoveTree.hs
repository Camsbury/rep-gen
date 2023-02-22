{-# LANGUAGE RankNTypes #-}
--------------------------------------------------------------------------------
module RepGen.MoveTree
  ( module RepGen.MoveTree
  ) where
--------------------------------------------------------------------------------
import RepGen.Type
import RepGen.Monad
import RepGen.MoveTree.Type
import RepGen.State.Type
import RepGen.Stats.Type
--------------------------------------------------------------------------------

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

-- | Nice fetcher for aggregate probability of a Node
fetchPAgg :: Vector Uci -> RGM Double
fetchPAgg ucis
  = throwMaybe ("Node doesn't exist at: " <> tshow ucis)
  <=< preuse $ moveTree . traverseUcis ucis . rgStats . probAgg

-- | Traversal of the valid children of a node
validChildrenT :: Traversal' TreeNode (Uci, TreeNode)
validChildrenT
  = responses
  . traversed
  . filtered (\x -> x ^. _2 . removed)

-- | Convenience to get all valid children as a list
collectValidChildren :: TreeNode -> [(Uci, TreeNode)]
collectValidChildren node
  = node
  ^.. responses
  . folded
  . filtered (\x -> x ^. _2 . removed . to not)
