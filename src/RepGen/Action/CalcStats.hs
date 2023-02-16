{-# LANGUAGE RankNTypes #-}
module RepGen.Action.CalcStats
  ( module RepGen.Action.CalcStats
  ) where
--------------------------------------------------------------------------------
import Prelude
import Control.Monad.State (get)
import Control.Lens (Lens', use)
import RepGen.Type
import RepGen.Monad
import RepGen.MoveTree.Type
import RepGen.Action.Type
import RepGen.State.Type
--------------------------------------------------------------------------------

weightedStat
  :: Lens' NodeStats (Maybe Double)
  -> NodeStats
  -> Double
weightedStat winsL ns = stat * p
  where
    stat = fromMaybe 0 $ ns ^. winsL
    p    = fromMaybe 0 $ ns ^. prob

childrenStat
  :: Vector (Uci, TreeNode)
  -> Lens' TreeNode NodeStats
  -> Lens' NodeStats (Maybe Double)
  -> Double
childrenStat children parentL statL
  = sum
  $ children
  ^.. folded
  . _2
  . parentL
  . to (weightedStat statL)

-- NOTE: could split to run per color
calcNodeStats
  :: Vector Uci
  -> Lens' TreeNode NodeStats
  -> RGM ()
calcNodeStats ucis statsLens = do
  parent
    <- throwMaybe ("No node exists for ucis: " <> intercalate ","  ucis)
    <=< preuse
    $ moveTree
    . traverseUcis ucis
  children
    <- use
    $ moveTree
    . traverseUcis ucis
    . responses
  let cWhite = childrenStat children statsLens $ whiteStats . wins
  let cWhiteAgg = childrenStat children statsLens $ whiteStats . winsAgg
  let cBlack = childrenStat children statsLens $ blackStats . wins
  let cBlackAgg = childrenStat children statsLens $ blackStats . winsAgg
  let pWhite = parent ^. statsLens . whiteStats . winsAgg
  let pBlack = parent ^. statsLens . blackStats . winsAgg
  moveTree . traverseUcis ucis . statsLens . whiteStats . wins
    .= ((\pW -> cWhiteAgg + (pW - cWhite)) <$> pWhite)
  moveTree . traverseUcis ucis . statsLens . blackStats . wins
    .= ((\pW -> cBlackAgg + (pW - cBlack)) <$> pBlack)

-- | Decrement the sum of the child probabilities
-- empty probabilities mean we don't want to take the child into account
-- so we use 0
probNonChild
  :: Vector (Uci, TreeNode)
  -> Double
probNonChild children
  = (1 -)
  . sum
  $ children
  ^.. folded
  . _2
  . lichessStats
  . prob
  . to (fromMaybe 0)

calcScore
  :: Vector Uci
  -> RGM ()
calcScore ucis = do
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
  let pnc = probNonChild children
  let cScoreAgg
        = sum
        $ children
        ^.. folded
        . _2
        . to (\n -> fromMaybe 0 (n ^. sharedStats . scoreAgg)
                 * fromMaybe 0 (n ^. lichessStats . prob))
  let pScore = parent ^. sharedStats . scoreAgg
  moveTree . traverseUcis ucis . sharedStats . scoreAgg
    .= ((\pS -> cScoreAgg + pnc * pS) <$> pScore)

runAction :: CalcStats -> RGM ()
runAction (CalcStats ucis) = do
  calcNodeStats ucis lichessStats
  calcNodeStats ucis mastersStats
  calcScore ucis
