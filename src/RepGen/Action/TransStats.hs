{-# LANGUAGE RankNTypes #-}
--------------------------------------------------------------------------------
module RepGen.Action.TransStats
  ( runAction
  ) where
--------------------------------------------------------------------------------

import RepGen.Monad
import RepGen.Type
import RepGen.State.Type
import RepGen.MoveTree
import RepGen.MoveTree.Type
import RepGen.Stats.Type
import RepGen.Strategy
--------------------------------------------------------------------------------
import qualified RepGen.MoveTree as MT
--------------------------------------------------------------------------------

setScore :: Maybe RGStat -> Vector Uci -> RGM ()
setScore Nothing _ = pure ()
setScore (Just s) ucis = do
  moveTree . MT.traverseUcis ucis . rgStats . rgScore . _Just . agg .= (s ^. agg)

setNodeStats
  :: Maybe NodeStats
  -> Vector Uci
  -> Lens' RGStats (Maybe NodeStats)
  -> Lens' NodeStats Double
  -> RGM ()
setNodeStats Nothing _ _ _ = pure ()
setNodeStats (Just s) ucis nodeStats aggStats = do
  moveTree . MT.traverseUcis ucis . rgStats . nodeStats . _Just . aggStats .= (s ^. aggStats)

runAction :: Vector Uci -> RGM ()
runAction ucis = do
  logDebugN $ "Transferring Stats for: " <> intercalate "," ucis
  children
    <- use
    $ moveTree
    . MT.traverseUcis ucis
    . to collectValidChildren
  (choiceUci, child) <- applyStrategy children
  -- NOTE: these are something like natural transformations?

  setScore (child ^. rgStats . rgScore) (ucis <> [choiceUci])
  setNodeStats
    (child ^. rgStats . lichessStats)
    (ucis <> [choiceUci]) lichessStats (whiteWins . agg)
  setNodeStats
    (child ^. rgStats . lichessStats)
    (ucis <> [choiceUci]) lichessStats (blackWins . agg)
  setNodeStats
    (child ^. rgStats . mastersStats)
    (ucis <> [choiceUci]) mastersStats (whiteWins . agg)
  setNodeStats
    (child ^. rgStats . mastersStats)
    (ucis <> [choiceUci]) mastersStats (blackWins . agg)



