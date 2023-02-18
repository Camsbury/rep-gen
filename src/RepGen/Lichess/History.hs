module RepGen.Lichess.History
  ( module RepGen.Lichess.History.Type
  , historicMovesMasters
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

-- | Convert masters params to query params
fromMastersParams :: UniversalParams -> Map Text Text
fromMastersParams params
  = mapFromList
  [ ("moves",    params ^. moveCount . to tshow)
  , ("fen",      params ^. fen)
  , ("topGames", "0")
  ]

-- | get historic Lichess moves for masters games
historicMovesMasters :: UniversalParams -> RGM RawStats
historicMovesMasters params = do
  dbPath <- view cachePath
  (statusCode, response)
    <- liftIO
    . Web.cachedGetRequest dbPath (baseUrl <> "masters")
    . fromMastersParams
    $ params
  case statusCode of
    200 -> throwEither
        . first pack
        . J.eitherDecode
        . fromString
        $ unpack response
    429 -> do
      liftIO $ C.threadDelay oneMinute
      historicMovesMasters params
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

