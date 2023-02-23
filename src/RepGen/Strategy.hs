--------------------------------------------------------------------------------
module RepGen.Strategy
  ( applyStrategy
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
  sComp <- strategicCompare <$> view strategy <*> view colorL
  opts <- throwMaybe "No moves to apply the strategy to!"
       $ fromNullable options
  let choice = minimumBy sComp opts
  -- logErrorN
  --   . ("Comparing the following children: " <>)
  --   . tshow
  --   $ view _1
  --   <$> options
  -- logErrorN
  --   . ("With white wins: " <>)
  --   . tshow
  --   $ options ^.. folded . _2 . rgStats . mastersStats . _Just . whiteWins . agg
  -- logErrorN
  --   . ("With black wins: " <>)
  --   . tshow
  --   $ options ^.. folded . _2 . rgStats . mastersStats . _Just . blackWins . agg
  pure choice

-- | Get the 'Ordering' needed to fulfill the chosen 'RGStrategy'
strategicCompare
  :: RGStrategy
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
