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
      { _whiteWins = RGStat white white,
        _blackWins = RGStat black black,
        _prob = (white + black + draws) / fromIntegral totalCount
      }
  )
  where
    white = fromIntegral $ rs ^. rawWhite
    black = fromIntegral $ rs ^. rawBlack
    draws = fromIntegral $ rs ^. rawDraw
