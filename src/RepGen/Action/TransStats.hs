{-# LANGUAGE RankNTypes #-}
--------------------------------------------------------------------------------
module RepGen.Action.TransStats
  ( runAction
  ) where
--------------------------------------------------------------------------------

import RepGen.Monad
import RepGen.Type
import RepGen.State.Type
import RepGen.MoveTree.Type
import RepGen.Stats.Type
--------------------------------------------------------------------------------
import qualified RepGen.MoveTree as MT
import qualified RepGen.State    as State
import qualified RepGen.Strategy as Strat
--------------------------------------------------------------------------------

runAction :: Vector Uci -> RGM ()
runAction ucis = do
  logDebugN $ "Transferring Stats for: " <> tshow ucis
  parent
    <- throwMaybe ("Parent doesn't exist at ucis: " <> tshow ucis)
    <=< preuse
    $ moveTree . MT.traverseUcis ucis
  let pFen = parent ^. nodeFen
  childrenInfo
    <- traverse State.collectInfo
    <=< use
    $ moveTree
    . MT.traverseUcis ucis
    . to MT.collectValidChildren

  appliedStrat <- Strat.applyStrategy ucis childrenInfo
  case appliedStrat of
    Just (_, (_, _, child)) -> do
      logDebugN "Children exist for transfer!"

      setScore (child ^. posStats . rgScore) pFen
      setNodeStats
        (child ^. posStats . lichessStats)
        pFen
        lichessStats
        (whiteWins . agg)
      setNodeStats
        (child ^. posStats . lichessStats)
        pFen
        lichessStats
        (blackWins . agg)
      setNodeStats
        (child ^. posStats . mastersStats)
        pFen
        mastersStats
        (whiteWins . agg)
      setNodeStats
        (child ^. posStats . mastersStats)
        pFen
        mastersStats
        (blackWins . agg)
    Nothing
      -> logDebugN
      $ "No sound candidates when transferring stats at: "
      <> tshow ucis

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
    . ixPTI fen
    . posStats
    . rgScore
    . _Just
  if isJust currScore
    then
      posToInfo
        . ixPTI fen
        . posStats
        . rgScore
        . _Just
        . agg
        .= (s ^. agg)
    else
      posToInfo
        . ixPTI fen
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
  . ixPTI fen
  . posStats
  . nodeStats
  . _Just
  . aggStats
  .= (s ^. aggStats)
