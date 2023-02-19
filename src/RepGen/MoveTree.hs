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

fetchPAgg :: Vector Uci -> RGM Double
fetchPAgg ucis
  = throwMaybe ("Node doesn't exist at: " <> intercalate "," ucis)
  <=< preuse $ moveTree . traverseUcis ucis . rgStats . probAgg
