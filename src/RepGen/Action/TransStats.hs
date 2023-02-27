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
import RepGen.Stats.Type
--------------------------------------------------------------------------------
import qualified RepGen.MoveTree as MT
import qualified RepGen.State    as State
import qualified RepGen.Strategy as Strat
--------------------------------------------------------------------------------

runAction :: Vector Uci -> RGM ()
runAction ucis = do
  logDebugN $ "Transferring Stats for: " <> tshow ucis
  childrenInfo
    <- traverse State.collectInfo
    <=< use
    $ moveTree
    . MT.traverseUcis ucis
    . to collectValidChildren

  when (isJust $ fromNullable childrenInfo) $ do
    logDebugN "Children exist for transfer!"
    (_, (fen, child)) <- Strat.applyStrategy childrenInfo

    setScore (child ^. posStats . rgScore) fen
    setNodeStats
      (child ^. posStats . lichessStats)
      fen
      lichessStats
      (whiteWins . agg)
    setNodeStats
      (child ^. posStats . lichessStats)
      fen
      lichessStats
      (blackWins . agg)
    setNodeStats
      (child ^. posStats . mastersStats)
      fen
      mastersStats
      (whiteWins . agg)
    setNodeStats
      (child ^. posStats . mastersStats)
      fen
      mastersStats
      (blackWins . agg)

setScore
  :: Maybe RGStat
  -> Fen
  -> RGM ()
setScore Nothing _ = pure ()
setScore (Just s) fen
  = do
  currScore
    <- preuse
    $ posToInfo
    . ix fen
    . posStats
    . rgScore
    . _Just
  if isJust currScore
    then
      posToInfo
        . ix fen
        . posStats
        . rgScore
        . _Just
        . agg
        .= (s ^. agg)
    else
      posToInfo
        . ix fen
        . posStats
        . rgScore
        .= Just s

setNodeStats
  :: Maybe NodeStats
  -> Fen
  -> Lens' RGStats (Maybe NodeStats)
  -> Lens' NodeStats Double
  -> RGM ()
setNodeStats Nothing _ _ _ = pure ()
setNodeStats (Just s) fen nodeStats aggStats
  = posToInfo
  . ix fen
  . posStats
  . nodeStats
  . _Just
  . aggStats
  .= (s ^. aggStats)
