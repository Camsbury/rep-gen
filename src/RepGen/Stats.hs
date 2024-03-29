{-# LANGUAGE RankNTypes #-}
--------------------------------------------------------------------------------
module RepGen.Stats where
--------------------------------------------------------------------------------
import RepGen.Monad
import RepGen.Type
import RepGen.Lichess.History.Type
import RepGen.State.Type
import RepGen.Stats.Type
--------------------------------------------------------------------------------

-- | Parse stats for algorithm use
parseStats :: RawStats -> [(Uci, (Double, NodeStats))]
parseStats rs =
  parseStatsMove (rs ^. rawTotal) <$> rs ^. rawStatsMoves

parseStatsMove :: Int -> RawStatsMove -> (Uci, (Double, NodeStats))
parseStatsMove totalCount rs =
  ( rs ^. rawUci
  , ( total /. totalCount
    , NodeStats
        { _whiteWins = mkRGStat whiteP
        , _blackWins = mkRGStat blackP
        , _playCount = total
        }
    )
  )
  where
    white = rs ^. rawWhite
    black = rs ^. rawBlack
    draws = rs ^. rawDraw
    total = white + black + draws
    whiteP = white /. total
    blackP = black /. total

-- | Inject positional stats into former move stats
updateParentNominal
  :: Fen
  -> Traversal' RGStats (Maybe NodeStats)
  -> RawStats
  -> RGM ()
updateParentNominal fen statsT rs = do
  posToInfo . ixPTI fen . posStats . statsT . _Just . whiteWins . nom
    .= whiteS ^. nom
  posToInfo . ixPTI fen . posStats . statsT . _Just . blackWins . nom
    .= blackS ^. nom
  where
    white = rs ^. whiteTotal
    black = rs ^. blackTotal
    total = rs ^. rawTotal
    whiteS = mkRGStat $ white /. total
    blackS = mkRGStat $ black /. total
