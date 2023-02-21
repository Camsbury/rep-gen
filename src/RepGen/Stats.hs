--------------------------------------------------------------------------------
module RepGen.Stats where
--------------------------------------------------------------------------------
import RepGen.Type
import RepGen.Lichess.History.Type
import RepGen.Stats.Type
--------------------------------------------------------------------------------

-- | Parse stats for algorithm use
parseStats :: RawStats -> [(Uci, NodeStats)]
parseStats rs =
  parseStatsMove (rs ^. rawTotal) <$> rs ^. rawStatsMoves

parseStatsMove :: Int -> RawStatsMove -> (Uci, NodeStats)
parseStatsMove totalCount rs =
  ( rs ^. rawUci
  , NodeStats
      { _whiteWins = mkRGStat whiteP
      , _blackWins = mkRGStat blackP
      , _prob      = total /. totalCount
      , _playCount = total
      }
  )
  where
    white = rs ^. rawWhite
    black = rs ^. rawBlack
    draws = rs ^. rawDraw
    total = white + black + draws
    whiteP = white /. total
    blackP = black /. total
