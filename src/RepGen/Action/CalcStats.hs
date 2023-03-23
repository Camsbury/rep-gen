{-# LANGUAGE RankNTypes #-}
module RepGen.Action.CalcStats
  ( runAction
  ) where
--------------------------------------------------------------------------------

import RepGen.Type
import RepGen.Config.Type
import RepGen.Monad
import RepGen.MoveTree
import RepGen.MoveTree.Type
import RepGen.State.Type
import RepGen.Stats.Type
--------------------------------------------------------------------------------
import qualified RepGen.MoveTree as MT
import qualified RepGen.State    as State
--------------------------------------------------------------------------------

-- | Calculate stats for a candidate node
-- given the weighted stats of its children
runAction :: Vector Uci -> RGM ()
runAction ucis = do
  logDebugN $ "Calculating Stats for: " <> tshow ucis
  node
    <- throwMaybe ("No node exists for ucis: " <> tshow  ucis)
    <=< preuse
    $ moveTree
    . MT.traverseUcis ucis

  useMasters <- view mastersP

  calcNodeStats node lichessStats
  when useMasters $ calcNodeStats node mastersStats
  calcScore node

-- | Calculate the win probabilites per color for a given node
-- adjusted for the known probabilities of the children nodes
-- NOTE: could split to run per color
calcNodeStats
  :: TreeNode
  -> Lens' RGStats (Maybe NodeStats)
  -> RGM ()
calcNodeStats parent statsLens = do
  let parentFen = parent ^. nodeFen
  let parentUcis = parent ^. uciPath
  parentStats
    <- throwMaybe ("No info exists for fen: " <> tshow parentFen)
    <=< preuse
    $ posToInfo . ixPTI parentFen . posStats
  -- let children = fromList $ parent ^.. validChildrenT
  let children = fromList $ parent ^.. nodeResponses . folded
  cWhite     <- childrenStat children statsLens $ whiteWins . nom
  cWhiteAgg  <- childrenStat children statsLens $ whiteWins . agg
  cBlack     <- childrenStat children statsLens $ blackWins . nom
  cBlackAgg  <- childrenStat children statsLens $ blackWins . agg
  let pWhite = parentStats ^? statsLens . _Just . whiteWins . nom
  let pBlack = parentStats ^? statsLens . _Just . blackWins . nom
  logDebugN
    $ "Setting Agg Stat for: "
    <> tshow parentUcis
  setAggStat
    ((\pW -> cWhiteAgg + (pW - cWhite)) <$> pWhite)
    parentFen statsLens (whiteWins . agg)
  setAggStat
    ((\pB -> cBlackAgg + (pB - cBlack)) <$> pBlack)
    parentFen statsLens (blackWins . agg)

-- | Calculate a stat weighting
weightedStat
  :: Lens' NodeStats Double
  -> NodeStats
  -> Double
weightedStat winsL ns = view winsL ns * view prob ns

setAggStat
  :: Maybe Double
  -> Fen
  -> Lens' RGStats (Maybe NodeStats)
  -> Lens' NodeStats Double
  -> RGM ()
setAggStat Nothing _ _ _ = pure ()
setAggStat (Just s) fen nodeStats aggStats = do
  posToInfo . ixPTI fen . posStats . nodeStats . _Just . aggStats .= s

-- | Extract a weighted statistic for child nodes
childrenStat
  :: Vector (Uci, TreeNode)
  -> Lens' RGStats (Maybe NodeStats)
  -> Lens' NodeStats Double
  -> RGM Double
childrenStat children parentL statL
  = fmap sum . traverse getStat $ children ^.. folded . _2 . nodeFen
  where
    getStat :: Fen -> RGM Double
    getStat fen
      = throwMaybe ("Info doesn't exist for fen: " <> tshow fen)
      <=< preuse
      $ posToInfo . ixPTI fen . posStats . parentL . _Just
      . to (weightedStat statL)

-- | Decrement the sum of the child probabilities
-- empty probabilities mean we don't want to take the child into account
-- so we use 0
probNonChild
  :: [(Uci, (Fen, PosInfo))]
  -> Double
probNonChild children
  = (1 -)
  . sum
  $ children
  ^.. folded
  . _2
  . _2
  . posStats
  . lichessStats
  . to (maybe 0 $ view prob)

setScore :: Maybe Double -> Fen -> RGM ()
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
        .= s
    else
      posToInfo
        . ixPTI fen
        . posStats
        . rgScore
        .= Just (mkRGStat s)

-- | Calculate the weighted score for a node given the chosen moves
-- for its children
calcScore
  :: TreeNode
  -> RGM ()
calcScore parent = do
  pTI <- use posToInfo
  let children = parent ^.. validChildrenT
  childrenInfo <- traverse State.collectInfo children
  let cScoreAgg
        = sum
        $ children
        ^.. folded
        . _2
        . to (\n -> fromMaybe 0 (pTI ^? ixPTI (n ^. nodeFen) . posStats . rgScore . _Just . agg)
                 * fromMaybe 0 (pTI ^? ixPTI (n ^. nodeFen) . posStats . lichessStats . _Just . prob))
  let pScore = pTI ^? ixPTI (parent ^. nodeFen) . posStats . rgScore . _Just . nom
  setScore ((\pS -> cScoreAgg + probNonChild childrenInfo * pS) <$> pScore) $ parent ^. nodeFen
