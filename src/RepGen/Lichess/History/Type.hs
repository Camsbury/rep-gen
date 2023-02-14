{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}
module RepGen.Lichess.History.Type where

import Prelude

import RepGen.Type
-- NOTE: makeLenses interrupts python for some reason,
-- so I have to manually make my lenses...
import Control.Lens (makeLenses, makeLensesFor)
import Data.Time.Calendar (Year(..), MonthOfYear(..))

data UniversalParams
  = UniversalParams
  { _moveCount :: Int -- moves
  , _fen       :: !Text
  } deriving (Eq, Show)
makeLenses ''UniversalParams

data GameSpeed
  = Bullet
  | Blitz
  | Rapid
  | Classical
  deriving (Eq, Show)

data LichessRating
  = L600
  | L1000
  | L1200
  | L1400
  | L1600
  | L1800
  | L2000
  | L2200
  | L2500
  deriving (Eq, Show)

data LichessParams
  = LichessParams
  { _ratings    :: Vector LichessRating
  , _speeds     :: Vector GameSpeed
  , _universals :: UniversalParams
  } deriving (Eq, Show)
makeLensesFor [("_", "lichess")] ''LichessParams

type PlayerName = Text

data PlayerParams
  = PlayerParams
  { _color      :: Color
  , _player     :: !PlayerName
  , _speeds     :: Vector GameSpeed
  , _universals :: UniversalParams
  , _since      :: (Year, MonthOfYear) -- default to Jan 1952
  } deriving (Eq, Show)
makeLensesFor [("_", "player")] ''PlayerParams
