{-# LANGUAGE RankNTypes #-}
--------------------------------------------------------------------------------
module RepGen.MoveTree
  ( module RepGen.MoveTree
  ) where
--------------------------------------------------------------------------------
import RepGen.Engine.Type
import RepGen.Monad
import RepGen.MoveTree.Type
import RepGen.Score.Type
import RepGen.State.Type
import RepGen.Stats.Type
import RepGen.Type
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
  = throwMaybe ("Node doesn't exist at: " <> tshow ucis)
  <=< preuse $ moveTree . traverseUcis ucis . probAgg

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

-- | Convenience function to update all the extra data needed on tree nodes
insertNodeInfo
  :: Bool
  -> Maybe Double
  -> [EngineCandidate]
  -> Double
  -> Double
  -> Vector Uci
  -> (Uci, (Double, Fen))
  -> RGM ()
insertNodeInfo isResps mBestScore nCands pAgg pPrune ucis (uci, (pMove, _)) = do
  if isResps
    then do
      moveTree . traverseUcis (snoc ucis uci) . bestScoreL .= (mBestScore <|> findBy uci nCands)
      moveTree . traverseUcis (snoc ucis uci) . probAgg .= pAgg * pMove
      moveTree . traverseUcis (snoc ucis uci) . probPrune .= pPrune * pMove
    else do
      moveTree . traverseUcis (snoc ucis uci) . bestScoreL .= (max <$> mBestScore <*> maxCandScore)
      moveTree . traverseUcis (snoc ucis uci) . probAgg .= pAgg
      moveTree . traverseUcis (snoc ucis uci) . probPrune .= pPrune
  where
    findBy _ [] = Nothing
    findBy u (ngn:rest)
      | u == ngn ^. ngnUci = Just $ ngn ^. ngnScore . scoreL
      | otherwise = findBy u rest
    maxCandScore = maximumMay $ nCands ^.. folded . ngnScore . scoreL
