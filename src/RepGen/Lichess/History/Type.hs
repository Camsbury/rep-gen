{-# LANGUAGE TemplateHaskell #-}
module RepGen.Lichess.History.Type where

import Prelude

import RepGen.Type
import Control.Lens (makeLenses)
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
  { _lichessRatings :: Vector LichessRating
  , _lichessSpeeds  :: Vector GameSpeed
  , _universals :: UniversalParams
  } deriving (Eq, Show)
makeLenses ''LichessParams

type PlayerName = Text

data PlayerParams
  = PlayerParams
  { _playerColor      :: Color
  , _player           :: !PlayerName
  , _playerSpeeds     :: Vector GameSpeed
  , _playerUniversals :: UniversalParams
  , _playerSince      :: (Year, MonthOfYear) -- default to Jan 1952
  } deriving (Eq, Show)
makeLenses ''PlayerParams
