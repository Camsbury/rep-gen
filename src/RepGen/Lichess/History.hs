module RepGen.Lichess.History where

import Prelude

import RepGen.Type
import Data.Time.Calendar (Year(..), MonthOfYear(..))

data GlobalParams
  = GlobalParams
  { moveCount :: Int -- moves
  , fen       :: Text
  } deriving (Eq, Show)

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
  { ratings        :: Vector LichessRating
  , lichessSpeeds  :: Vector GameSpeed
  , lichessGlobals :: GlobalParams
  } deriving (Eq, Show)

type PlayerName = Text

data PlayerParams
  = PlayerParams
  { color         :: Color
  , player        :: PlayerName
  , playerSpeeds  :: Vector GameSpeed
  , playerGlobals :: GlobalParams
  , since         :: (Year, MonthOfYear) -- default to Jan 1952
  } deriving (Eq, Show)
