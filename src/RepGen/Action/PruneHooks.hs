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
import RepGen.Stats.Type
--------------------------------------------------------------------------------

runAction :: Vector Uci -> RGM ()
runAction ucis = do
  logDebugN $ "Running Prune Hooks for: " <> tshow ucis
  mpa <- view minProbAgg
  moveTree
    . traverseUcis ucis
    . responses
    . traversed
    . filtered (\x -> x ^. _2 . rgStats . probAgg < mpa)
    . _2
    . removed
    .= True
  children <- use $ moveTree . traverseUcis ucis . to collectValidChildren

  let actions = toActions (children ^.. folded . _2)
  -- logDebugN $ "Actions: " <> tshow actions
  actionStack %= (actions ++)

toActions :: [TreeNode] -> [RGAction]
toActions nodes = addHooks =<< nodes

addHooks
  :: TreeNode
  -> [RGAction]
addHooks node
  =  doAddHooks (RGACalcStats, RGATransStats) node
  ++ [RGATransStats ucis, RGAPruneCands ucis]
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
