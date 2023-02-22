{-# LANGUAGE RankNTypes #-}
module RepGen.Action.CalcStats
  ( runAction
  ) where
--------------------------------------------------------------------------------

import RepGen.Type
import RepGen.Monad
import RepGen.MoveTree
import RepGen.MoveTree.Type
import RepGen.State.Type
import RepGen.Stats.Type
--------------------------------------------------------------------------------
import qualified RepGen.MoveTree as MT
--------------------------------------------------------------------------------

-- | Calculate a stat weighting
weightedStat
  :: Lens' NodeStats Double
  -> NodeStats
  -> Double
weightedStat winsL ns = view winsL ns * view prob ns

-- | Extract a weighted statistic for child nodes
childrenStat
  :: Vector (Uci, TreeNode)
  -> Lens' RGStats (Maybe NodeStats)
  -> Lens' NodeStats Double
  -> Double
childrenStat children parentL statL
  = sum
  $ children
  ^.. folded
  . _2
  . rgStats
  . parentL
  . _Just
  . to (weightedStat statL)

setAggStat
  :: Maybe Double
  -> Vector Uci
  -> Lens' RGStats (Maybe NodeStats)
  -> Lens' NodeStats Double
  -> RGM ()
setAggStat Nothing _ _ _ = pure ()
setAggStat (Just s) ucis nodeStats aggStats = do
  moveTree . MT.traverseUcis ucis . rgStats . nodeStats . _Just . aggStats .= s

-- | Calculate the win probabilites per color for a given node
-- adjusted for the known probabilities of the children nodes
-- NOTE: could split to run per color
calcNodeStats
  :: Vector Uci
  -> Lens' RGStats (Maybe NodeStats)
  -> RGM ()
calcNodeStats ucis statsLens = do
  parent
    <- throwMaybe ("No node exists for ucis: " <> intercalate ","  ucis)
    <=< preuse
    $ moveTree
    . MT.traverseUcis ucis
  let children = fromList $ parent ^.. validChildrenT
  let cWhite :: Double = childrenStat children statsLens $ whiteWins . nom
  let cWhiteAgg :: Double = childrenStat children statsLens $ whiteWins . agg
  let cBlack = childrenStat children statsLens $ blackWins . nom
  let cBlackAgg = childrenStat children statsLens $ blackWins . agg
  let pWhite :: Maybe Double = parent ^? rgStats . statsLens . _Just . whiteWins . agg
  let pBlack = parent ^? rgStats . statsLens . _Just . blackWins . agg
  setAggStat
    ((\pW -> cWhiteAgg + (pW - cWhite)) <$> pWhite)
    ucis statsLens (whiteWins . nom)
  setAggStat
    ((\pB -> cBlackAgg + (pB - cBlack)) <$> pBlack)
    ucis statsLens (blackWins . nom)

-- | Decrement the sum of the child probabilities
-- empty probabilities mean we don't want to take the child into account
-- so we use 0
probNonChild
  :: [(Uci, TreeNode)]
  -> Double
probNonChild children
  = (1 -)
  . sum
  $ children
  ^.. folded
  . _2
  . rgStats
  . lichessStats
  . to (maybe 0 $ view prob)

setScore :: Maybe Double -> Vector Uci -> RGM ()
setScore Nothing _ = pure ()
setScore (Just s) ucis = do
  moveTree . MT.traverseUcis ucis . rgStats . rgScore . _Just . agg .= s

-- | Calculate the weighted score for a node given the chosen moves
-- for its children
calcScore
  :: Vector Uci
  -> RGM ()
calcScore ucis = do
  parent
    <- throwMaybe ("No node exists for ucis: " <> intercalate "," ucis)
    <=< preuse
    $ moveTree
    . MT.traverseUcis ucis
  let children = parent ^.. validChildrenT
  let cScoreAgg
        = sum
        $ children
        ^.. folded
        . _2
        . to (\n -> fromMaybe 0 (n ^? rgStats . rgScore . _Just . agg)
                 * fromMaybe 0 (n ^? rgStats . lichessStats . _Just . prob))
  let pScore = parent ^? rgStats . rgScore . _Just . agg
  setScore ((\pS -> cScoreAgg + probNonChild children * pS) <$> pScore) ucis

-- | Calculate stats for a candidate node
-- given the weighted stats of its children
runAction :: Vector Uci -> RGM ()
runAction ucis = do
  logDebugN $ "Calculating Stats for: " <> intercalate "," ucis
  calcNodeStats ucis lichessStats
  calcNodeStats ucis mastersStats
  calcScore ucis
