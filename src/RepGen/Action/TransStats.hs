{-# LANGUAGE RankNTypes #-}
--------------------------------------------------------------------------------
module RepGen.Action.TransStats
  ( module RepGen.Action.TransStats
  ) where
--------------------------------------------------------------------------------

import RepGen.Monad
import RepGen.Type
import RepGen.Action.Type
import RepGen.State.Type
import RepGen.MoveTree.Type
import RepGen.Strategy
--------------------------------------------------------------------------------

setScore :: Maybe RGStat -> Vector Uci -> RGM ()
setScore Nothing _ = pure ()
setScore (Just s) ucis = do
  moveTree . traverseUcis ucis . rgStats . score . _Just . agg .= (s ^. agg)

setNodeStats
  :: Maybe NodeStats
  -> Vector Uci
  -> Lens' RGStats (Maybe NodeStats)
  -> Lens' NodeStats Double
  -> RGM ()
setNodeStats Nothing _ _ _ = pure ()
setNodeStats (Just s) ucis nodeStats aggStats = do
  moveTree . traverseUcis ucis . rgStats . nodeStats . _Just . aggStats .= (s ^. aggStats)

runAction :: TransStats -> RGM ()
runAction (TransStats ucis) = do
  children
    <- use
    $ moveTree
    . traverseUcis ucis
    . responses
  choiceUci <- applyStrategy children
  child -- NOTE: this is just getting the value of the strategy key, maybe just return both?
    <- throwMaybe "impossible state!"
    $ children
    ^? folded
    . filtered (\(x, _) -> x == choiceUci)
    . _2
  -- NOTE: these are something like natural transformations?

  setScore (child ^. rgStats . score) (ucis <> [choiceUci])
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



