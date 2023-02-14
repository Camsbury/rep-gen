module RepGen.Lichess.History
  ( module RepGen.Lichess.History.Type
  , historicMovesMasters
  ) where

import Prelude

import Control.Lens.Operators
import Control.Lens.Combinators
import RepGen.Lichess.History.Type

-- hitting https://explorer.lichess.ovh/{masters,lichess,player}

--- type has uci, white, black, play-count, prob

baseUrl :: Text
baseUrl = "https://explorer.lichess.ovh/"

-- | Convert masters params to query params
fromMastersParams :: UniversalParams -> Map Text Text
fromMastersParams params
  = mapFromList
  [ ("moves",    params ^. moveCount . to tshow)
  , ("fen",      params ^. fen)
  , ("topGames", "0")
  ]

-- | get historic Lichess moves for masters games
historicMovesMasters :: UniversalParams -> IO Text
historicMovesMasters
  = getRequest (baseUrl <> "masters") . fromMastersParams

