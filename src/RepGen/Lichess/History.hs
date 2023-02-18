module RepGen.Lichess.History
  ( module RepGen.Lichess.History.Type
  , historicMoves
  ) where

import RepGen.Config.Type
import RepGen.Lichess.History.Type
import RepGen.Monad
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
  , ("fen",      params ^. fen)
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

