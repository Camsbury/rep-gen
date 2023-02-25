{-# LANGUAGE RankNTypes #-}
--------------------------------------------------------------------------------
module RepGen.Stats where
--------------------------------------------------------------------------------
import RepGen.Monad
import RepGen.Type
import RepGen.Lichess.History.Type
import RepGen.MoveTree
import RepGen.MoveTree.Type
import RepGen.State.Type
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

-- | Inject positional stats into former move stats
updateParentNominal
  :: Vector Uci
  -> Traversal' RGStats (Maybe NodeStats)
  -> RawStats
  -> RGM ()
updateParentNominal ucis statsT rs = do
  moveTree . traverseUcis ucis . rgStats . statsT . _Just . whiteWins .= whiteS
  moveTree . traverseUcis ucis . rgStats . statsT . _Just . blackWins .= blackS
  where
    white = rs ^. whiteTotal
    black = rs ^. blackTotal
    total = rs ^. rawTotal
    whiteS = mkRGStat $ white /. total
    blackS = mkRGStat $ black /. total
