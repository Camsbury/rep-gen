module RepGen.Lichess.History
  ( module RepGen.Lichess.History.Type
  , lichessMoves
  , maybeMastersMoves
  ) where

import RepGen.Config.Type
import RepGen.Lichess.History.Type
import RepGen.Monad
import RepGen.Stats
import RepGen.Stats.Type
import RepGen.Type
import qualified Control.Concurrent as C
import qualified Data.Aeson as J
import qualified Web

-- hitting https://explorer.lichess.ovh/{masters,lichess,player}

--- type has uci, white, black, play-count, prob

baseUrl :: Text
baseUrl = "https://explorer.lichess.ovh/"

-- | One minute in microseconds
oneMinute :: Int
oneMinute = 60 * 1000 * 1000

class HistoricFetchable a where
  historicMoves :: a -> RGM RawStats

instance HistoricFetchable UniversalParams where
  historicMoves params
    = fetchMovesFor (fromMastersParams params) "masters"

-- | Convert masters params to query params
fromMastersParams :: UniversalParams -> Map Text Text
fromMastersParams params
  = mapFromList
  [ ("moves",    params ^. moveCount . to tshow)
  , ("fen",      params ^. fenParam . fenL)
  , ("topGames", "0")
  ]

instance HistoricFetchable LichessParams where
  historicMoves params
    = fetchMovesFor (fromLichessParams params) "lichess"

-- | Convert lichess params to query params
fromLichessParams :: LichessParams -> Map Text Text
fromLichessParams params
  = mapFromList
  [ ( "ratings"
    ,  params ^. lichessRatings . to (intercalate "," . fmap ratingText)
    )
  , ( "speeds"
    , params ^. lichessSpeeds . to (intercalate "," . fmap speedText)
    )
  , ("recentGames", "0")
  ] <> fromMastersParams (params ^. universals)

-- | get historic Lichess moves for masters games
fetchMovesFor :: Map Text Text -> Text -> RGM RawStats
fetchMovesFor params path = do
  dbPath <- view cachePath
  (statusCode, response)
    <- liftIO
    . Web.cachedGetRequest dbPath (baseUrl <> path)
    $ params
  case statusCode of
    200 -> throwEither
        . first pack
        . J.eitherDecode
        . fromString
        $ unpack response
    429 -> do
      liftIO $ C.threadDelay oneMinute
      fetchMovesFor params path
    404 -> throwError
        ( "404 Not found for params: "
        <> tshow params
        )
    code -> throwError
         ( "HTTP error code: "
         <> tshow code
         <> " for params: "
         <> tshow params
         )

filterMinTotal :: RawStats -> RGM (Maybe RawStats)
filterMinTotal rs = do
  mtm <- view minTotalMasters
  pure $ if rs ^. rawTotal > mtm
    then Just rs
    else Nothing

getLichessParams :: Fen -> RGM LichessParams
getLichessParams fen = do
  hmc <- view $ historyConfig . historyMoveCount
  speeds <- view $ historyConfig . historySpeeds
  ratings <- view $ historyConfig . historyRatings
  let ups = UniversalParams hmc fen
  pure $ LichessParams ratings speeds ups

lichessMoves :: Fen -> RGM [(Uci, NodeStats)]
lichessMoves = fmap parseStats . historicMoves <=< getLichessParams

getMastersParams :: Fen -> RGM UniversalParams
getMastersParams fen = do
  hmc <- view $ historyConfig . historyMoveCount
  pure $ UniversalParams hmc fen

-- | Fetches masters moves if they meet our criteria
maybeMastersMoves :: Fen -> RGM (Maybe [(Uci, NodeStats)])
maybeMastersMoves fen = do
  useM <- view mastersP
  mps <- getMastersParams fen
  if useM
    then fmap (fmap parseStats) . filterMinTotal =<< historicMoves mps
    else pure Nothing
