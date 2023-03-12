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
  -> (Uci, (Fen, PosInfo))
  -> RGM ()
insertNodeInfo isResps bestScore nCands pAgg pPrune ucis (uci, (_, pInfo)) = do
  moveProb
    <- throwMaybe ("lichess stats missing for ucis: " <> tshow (snoc ucis uci))
    $ pInfo ^? posStats . lichessStats . _Just . prob
  moveTree . traverseUcis (snoc ucis uci) . bestScoreL .= (bestScore <|> findBy uci nCands)
  if isResps
    then do
      moveTree . traverseUcis (snoc ucis uci) . probAgg .= pAgg * moveProb
      moveTree . traverseUcis (snoc ucis uci) . probPrune .= pPrune * moveProb
    else do
      moveTree . traverseUcis (snoc ucis uci) . probAgg .= pAgg
      moveTree . traverseUcis (snoc ucis uci) . probPrune .= pPrune
  where
    findBy _ [] = Nothing
    findBy u (ngn:rest)
      | u == ngn ^. ngnUci = Just $ ngn ^. ngnScore . scoreL
      | otherwise = findBy u rest
