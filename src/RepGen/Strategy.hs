--------------------------------------------------------------------------------
module RepGen.Strategy
  ( applyStrategy
  , strategicFilter
  , strategicCompare
  ) where
--------------------------------------------------------------------------------

import RepGen.Config.Type
import RepGen.Monad
import RepGen.MoveTree.Type
import RepGen.Strategy.Type
import RepGen.Stats.Type
import RepGen.Type
--------------------------------------------------------------------------------

-- | Apply a strategy to select the best move option
applyStrategy :: [(Uci, TreeNode)] -> RGM (Uci, TreeNode)
applyStrategy options = do
  sats <- view $ strategy . satisficers
  sComp <- strategicCompare <$> view (strategy . optimizer) <*> view colorL
  opts <- throwMaybe "No moves to apply the strategy to!"
       . fromNullable
       $ strategicFilter sats options
  let choice = minimumBy sComp opts
  -- logInfoN
  --   . ("Comparing the following children: " <>)
  --   . tshow
  --   $ view _1
  --   <$> options
  -- logInfoN
  --   . ("With white wins: " <>)
  --   . tshow
  --   $ options ^.. folded . _2 . rgStats . lichessStats . _Just . whiteWins . agg
  -- logInfoN
  --   . ("With black wins: " <>)
  --   . tshow
  --   $ options ^.. folded . _2 . rgStats . lichessStats . _Just . blackWins . agg
  pure choice

-- | Filter options based on 'RGSatisficers'
strategicFilter
  :: RGSatisficers
  -> [(Uci, TreeNode)]
  -> [(Uci, TreeNode)]
strategicFilter sats opts
  = if sats ^. engineFilter . engineP
    then
      maybe opts toFiltered maybeBestScore
    else
      opts
  where
    maybeBestScore
      = maximumMay
      $ opts ^.. folded . _2 . rgStats . rgScore . _Just . agg
    toFiltered bestScore = filter (allowable bestScore) opts
    aLoss = sats ^. engineFilter . engineAllowableLoss
    allowable bestScore opt
      = maybe False (\x -> x > (bestScore * aLoss))
      $ opt ^? _2 . rgStats . rgScore . _Just . agg


-- | Get the 'Ordering' needed to fulfill the chosen 'RGOptimizer'
strategicCompare
  :: RGOptimizer
  -> Color
  -> (Uci, TreeNode)
  -> (Uci, TreeNode)
  -> Ordering
strategicCompare MaxWinOverLoss = maxWinOverLoss
strategicCompare MinLoss        = minLoss

-- | Comparison for MinLoss
minLoss :: Color -> (Uci, TreeNode) -> (Uci, TreeNode) -> Ordering
minLoss c (_, a) (_, b) = fromMaybe EQ $ compM <|> comp
  where
    stat x pWins cStats = x ^? rgStats . cStats . _Just . pWins c . agg
    compM = compare
      <$> stat a oppWins mastersStats
      <*> stat b oppWins mastersStats
    comp = compare
      <$> stat a oppWins lichessStats
      <*> stat b oppWins lichessStats

-- | Comparison for MaxWinOverLoss
maxWinOverLoss :: Color -> (Uci, TreeNode) -> (Uci, TreeNode) -> Ordering
maxWinOverLoss c (_, a) (_, b) = fromMaybe EQ $ compM <|> comp
  where
    stat x pWins cStats = x ^? rgStats . cStats . _Just . pWins c . agg
    lossWinMA = do
      lm <- stat a oppWins mastersStats
      wm <- stat a myWins mastersStats
      lm /? wm
    lossWinMB = do
      lm <- stat b oppWins mastersStats
      wm <- stat b myWins mastersStats
      lm /? wm
    lossWinA = do
      l <- stat a oppWins lichessStats
      w <- stat a myWins lichessStats
      l /? w
    lossWinB = do
      l <- stat b oppWins lichessStats
      w <- stat b myWins lichessStats
      l /? w
    compM = compare <$> lossWinMA <*> lossWinMB
    comp = compare <$> lossWinA <*> lossWinB
