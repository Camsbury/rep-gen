module RepGen.Lichess.History
  ( module RepGen.Lichess.History.Type
  , historicMovesMasters
  ) where

import Prelude

import Control.Lens.Operators
import Control.Lens.Combinators
import RepGen.Lichess.History.Type
import qualified Control.Concurrent as C

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
historicMovesMasters :: UniversalParams -> IO (Maybe Text)
historicMovesMasters params = do
  (statusCode, response)
    <- getRequest (baseUrl <> "masters") . fromMastersParams $ params
  case statusCode of
    200 -> pure $ Just response
    429 -> do
      C.threadDelay oneMinute
      historicMovesMasters params
    404 -> pure Nothing -- TODO: update this to put errors into our app monad
    _   -> pure Nothing -- TODO: update this to put errors into our app monad

