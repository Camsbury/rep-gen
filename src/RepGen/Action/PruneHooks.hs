{-# LANGUAGE RankNTypes #-}
--------------------------------------------------------------------------------
module RepGen.Action.PruneHooks
  ( module RepGen.Action.PruneHooks
  ) where
--------------------------------------------------------------------------------
import RepGen.Action.Type
import RepGen.Config.Type
import RepGen.Monad
import RepGen.Type
import RepGen.MoveTree
import RepGen.MoveTree.Type
import RepGen.State.Type
--------------------------------------------------------------------------------

runAction :: Vector Uci -> RGM ()
runAction ucis = do
  logDebugN $ "Running Prune Hooks for: " <> tshow ucis
  mpa <- view minProbAgg
  children <- use
    $ moveTree
    . traverseUcis ucis
    . to (collectFilteredChildren
           ( \x ->
               maybe False (mpa <) $ x ^? _2 . probAgg
           )
         )

  -- setting removed true on children who aren't probable enough
  moveTree
    . traverseUcis ucis
    . nodeResponses
    . traversed
    . filtered (\x -> (x ^. _1) `notElem` (children ^.. folded . _1))
    . _2
    . removed
    .= True

  let actions = toActions (children ^.. folded . _2)
  actionStack %= (actions ++)

toActions :: [TreeNode] -> [RGAction]
toActions nodes = addHooks =<< nodes

addHooks
  :: TreeNode
  -> [RGAction]
addHooks node
  =  doAddHooks (RGATransStats, RGACalcStats) node
  ++ [RGAPruneCands ucis]
  where
    ucis = node ^. uciPath

doAddHooks
  :: (Vector Uci -> RGAction, Vector Uci -> RGAction)
  -> TreeNode
  -> [RGAction]
doAddHooks mkas@(mkAction, _) node =
  (doAddHooks (swap mkas) =<< children) <> [mkAction ucis]
  where
    ucis = node ^. uciPath
    children = node ^.. validChildrenT . _2
