{-# LANGUAGE RankNTypes #-}
--------------------------------------------------------------------------------
module RepGen.Strategy
  ( applyStrategy
  , strategicFilter
  , strategicCompare
  ) where
--------------------------------------------------------------------------------

import RepGen.Config.Type
import RepGen.Monad
import RepGen.Strategy.Type
import RepGen.State.Type
import RepGen.Stats.Type
import RepGen.Type
--------------------------------------------------------------------------------

-- | Apply a strategy to select the best move option
applyStrategy :: [(Uci, (Fen, PosInfo))] -> RGM (Maybe (Uci, (Fen, PosInfo)))
applyStrategy options = do
  sats <- view $ strategy . satisficers
  sComp <- strategicCompare <$> view (strategy . optimizer) <*> view colorL
  let opts = strategicFilter sats options
  let choice = minimumBy sComp <$> fromNullable opts
  pure choice

-- | Filter options based on 'RGSatisficers'
strategicFilter
  :: RGSatisficers
  -> [(Uci, (Fen, PosInfo))]
  -> [(Uci, (Fen, PosInfo))]
strategicFilter sats opts
  = if sats ^. engineFilter . engineP
    then
      maybe opts toFiltered maybeBestScore
    else
      opts
  where
    maybeBestScore
      = maximumMay
      $ opts ^.. folded . _2 . _2 . posStats . bestScoreL . _Just
    toFiltered bestScore = filter (allowable bestScore) opts
    aLoss = sats ^. engineFilter . engineAllowableLoss
    allowable bestScore opt
      = maybe False (\x -> x >= (bestScore * aLoss))
      $ opt ^? _2 . _2 . posStats . rgScore . _Just . nom

-- | Get the 'Ordering' needed to fulfill the chosen 'RGOptimizer'
strategicCompare
  :: RGOptimizer
  -> Color
  -> (Uci, (Fen, PosInfo))
  -> (Uci, (Fen, PosInfo))
  -> Ordering
strategicCompare MaxWinOverLoss = maxWinOverLoss
strategicCompare MinLoss        = minLoss


-- | Comparison for MinLoss
minLoss :: Color -> (Uci, (Fen, PosInfo)) -> (Uci, (Fen, PosInfo)) -> Ordering
minLoss c (_, (_, a)) (_, (_, b)) = fromMaybe EQ $ compM <|> comp
  where
    stat
      :: RGStats
      -> (Color -> Getter NodeStats RGStat)
      -> Getter RGStats (Maybe NodeStats)
      -> Maybe Double
    stat x pWins cStats
      = upperConf x cStats
      $ x ^? cStats . _Just . pWins c . agg
    compM = compare
      <$> stat (a ^. posStats) oppWins mastersStats
      <*> stat (b ^. posStats) oppWins mastersStats
    comp = compare
      <$> stat (a ^. posStats) oppWins lichessStats
      <*> stat (b ^. posStats) oppWins lichessStats

-- | Comparison for MaxWinOverLoss
maxWinOverLoss :: Color -> (Uci, (Fen, PosInfo)) -> (Uci, (Fen, PosInfo)) -> Ordering
maxWinOverLoss c (_, (_, a)) (_, (_, b)) = fromMaybe EQ $ compM <|> comp
  where
    stat
      :: RGStats
      -> (Color -> Getter NodeStats RGStat)
      -> Getter RGStats (Maybe NodeStats)
      -> Maybe Double
    stat x pWins cStats
      = x ^? cStats . _Just . pWins c . agg
    lossWinMA = do
      lm <- upperConf (a ^. posStats) mastersStats
         $ stat (a ^. posStats) oppWins mastersStats
      wm <- lowerConf (a ^. posStats) mastersStats
         $ stat (a ^. posStats) myWins mastersStats
      lm /? wm
    lossWinMB = do
      lm <- upperConf (b ^. posStats) mastersStats
         $ stat (b ^. posStats) oppWins mastersStats
      wm <- lowerConf (b ^. posStats) mastersStats
         $ stat (b ^. posStats) myWins mastersStats
      lm /? wm
    lossWinA = do
      l <- upperConf (a ^. posStats) lichessStats
        $ stat (a ^. posStats) oppWins lichessStats
      w <- lowerConf (a ^. posStats) lichessStats
        $ stat (a ^. posStats) myWins lichessStats
      l /? w
    lossWinB = do
      l <- upperConf (b ^. posStats) lichessStats
        $ stat (b ^. posStats) oppWins lichessStats
      w <- lowerConf (b ^. posStats) lichessStats
        $ stat (b ^. posStats) myWins lichessStats
      l /? w
    compM = compare <$> lossWinMA <*> lossWinMB
    comp = compare <$> lossWinA <*> lossWinB

-- | Calculate the upper end of the confidence interval using the method from:
-- https://www.statology.org/standard-error-of-proportion/
lowerConf
  :: RGStats
  -> Getter RGStats (Maybe NodeStats)
  -> Maybe Double
  -> Maybe Double
lowerConf x cStats mStat
  = do
    n    <- x ^? cStats . _Just . playCount . to fromIntegral
    stat <- mStat
    let z95    = 1.96
        stdErr = sqrt $ stat * (1 - stat) / n
    pure $ stat - z95 * stdErr

-- | Calculate the upper end of the confidence interval using the method from:
-- https://www.statology.org/standard-error-of-proportion/
upperConf
  :: RGStats
  -> Getter RGStats (Maybe NodeStats)
  -> Maybe Double
  -> Maybe Double
upperConf x cStats mStat
  = do
    n    <- x ^? cStats . _Just . playCount . to fromIntegral
    stat <- mStat
    let z95    = 1.96
        stdErr = sqrt $ stat * (1 - stat) / n
    pure $ stat + z95 * stdErr
