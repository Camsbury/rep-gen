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
      . nodeResponses
      . traversed
      . filtered (\(x, _) -> x == uci)
      . _2

-- | Nice fetcher for aggregate probability of a Node
fetchPAgg :: Vector Uci -> RGM Double
fetchPAgg ucis
  = do
    fen
      <- throwMaybe ("Node doesn't exist at: " <> tshow ucis)
      <=< preuse $ moveTree . traverseUcis ucis . nodeFen
    throwMaybe ("Stats don't exist for: " <> tshow ucis)
      <=< preuse $ posToInfo . ix fen . posStats . probAgg

-- | Traversal of the valid children of a node
validChildrenT :: Traversal' TreeNode (Uci, TreeNode)
validChildrenT
  = nodeResponses
  . traversed
  . filtered (\x -> x ^. _2 . removed . to not)

-- | Convenience to get all valid children as a list
collectValidChildren :: TreeNode -> [(Uci, TreeNode)]
collectValidChildren node
  = node
  ^.. nodeResponses
  . folded
  . filtered (\x -> x ^. _2 . removed . to not)

-- | Convenience to get all filtered children as a list
collectFilteredChildren :: ((Uci, TreeNode) -> Bool) -> TreeNode -> [(Uci, TreeNode)]
collectFilteredChildren f node
  = node
  ^.. nodeResponses
  . folded
  . filtered f
