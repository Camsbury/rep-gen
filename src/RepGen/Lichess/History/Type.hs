{-# LANGUAGE TemplateHaskell #-}
module RepGen.Lichess.History.Type where

import Prelude

import RepGen.Type
import Control.Lens (makeFieldsNoPrefix)
import Data.Time.Calendar (Year(..), MonthOfYear(..))

data UniversalParams
  = UniversalParams
  { _moveCount :: Int -- moves
  , _fen       :: Text
  } deriving (Eq, Show)
makeFieldsNoPrefix ''UniversalParams

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
  { _ratings        :: Vector LichessRating
  , _lichessSpeeds  :: Vector GameSpeed
  , _lichessGlobals :: UniversalParams
  } deriving (Eq, Show)
makeFieldsNoPrefix ''LichessParams

type PlayerName = Text

data PlayerParams
  = PlayerParams
  { _color         :: Color
  , _player        :: PlayerName
  , _playerSpeeds  :: Vector GameSpeed
  , _playerGlobals :: UniversalParams
  , _since         :: (Year, MonthOfYear) -- default to Jan 1952
  } deriving (Eq, Show)
makeFieldsNoPrefix ''PlayerParams
