--------------------------------------------------------------------------------
module RepGen.Action.TransStats
  ( module RepGen.Action.TransStats
  ) where
--------------------------------------------------------------------------------
import Prelude
import RepGen.Monad
import RepGen.Type
import RepGen.Action.Type
import RepGen.State.Type
import RepGen.MoveTree.Type
import RepGen.Strategy
--------------------------------------------------------------------------------

runAction :: TransStats -> RGM ()
runAction (TransStats ucis) = do
  parent
    <- throwMaybe ("No node exists for ucis: " <> intercalate "," ucis)
    <=< preuse
    $ moveTree
    . traverseUcis ucis
  children
    <- use
    $ moveTree
    . traverseUcis ucis
    . responses
  choiceUci <- applyStrategy children
  child
    <- throwMaybe "impossible state!"
    $ children
    ^? folded
    . filtered (\(x, y) -> x == choiceUci)
    . _2
  moveTree . traverseUcis (ucis <> [choiceUci]) . sharedStats . scoreAgg .=
    (child ^. sharedStats . scoreAgg)
  moveTree . traverseUcis (ucis <> [choiceUci]) . lichessStats . whiteStats . winsAgg .=
    (child ^. lichessStats . whiteStats . winsAgg)
  moveTree . traverseUcis (ucis <> [choiceUci]) . lichessStats . blackStats . winsAgg .=
    (child ^. lichessStats . blackStats . winsAgg)
  moveTree . traverseUcis (ucis <> [choiceUci]) . mastersStats . whiteStats . winsAgg .=
    (child ^. mastersStats . whiteStats . winsAgg)
  moveTree . traverseUcis (ucis <> [choiceUci]) . mastersStats . blackStats . winsAgg .=
    (child ^. mastersStats . blackStats . winsAgg)



