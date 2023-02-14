{-# LANGUAGE TemplateHaskell #-}
module RepGen.Lichess.History.Type where

import Prelude

import RepGen.Type
-- NOTE: makeLenses interrupts python for some reason,
-- so I have to manually make my lenses...
import Control.Lens (Lens', lens)
import Data.Time.Calendar (Year(..), MonthOfYear(..))

data UniversalParams
  = UniversalParams
  { _moveCount :: Int -- moves
  , _fen       :: !Text
  } deriving (Eq, Show)
-- makeFieldsNoPrefix ''UniversalParams

moveCount :: Lens' UniversalParams Int
moveCount
  = lens  _moveCount (\params moveCount -> params { _moveCount = moveCount})

fen :: Lens' UniversalParams Text
fen = lens _fen (\params fen -> params { _fen = fen})

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
  { _ratings           :: Vector LichessRating
  , _lichessSpeeds     :: Vector GameSpeed
  , _lichessUniversals :: UniversalParams
  } deriving (Eq, Show)
-- makeFieldsNoPrefix ''LichessParams

ratings :: Lens' LichessParams (Vector LichessRating)
ratings = lens _ratings (\params ratings -> params { _ratings = ratings})

lichessSpeeds :: Lens' LichessParams (Vector GameSpeed)
lichessSpeeds = lens _lichessSpeeds (\params lichessSpeeds ->
                                       params { _lichessSpeeds = lichessSpeeds})

lichessUniversals :: Lens' LichessParams UniversalParams
lichessUniversals
  = lens _lichessUniversals (\params lichessUniversals ->
                               params { _lichessUniversals = lichessUniversals})

type PlayerName = Text

data PlayerParams
  = PlayerParams
  { _color            :: Color
  , _player           :: !PlayerName
  , _playerSpeeds     :: Vector GameSpeed
  , _playerUniversals :: UniversalParams
  , _since            :: (Year, MonthOfYear) -- default to Jan 1952
  } deriving (Eq, Show)
-- makeFieldsNoPrefix ''PlayerParams

color :: Lens' PlayerParams Color
color = lens _color (\params color -> params { _color = color})

player :: Lens' PlayerParams PlayerName
player = lens _player (\params player -> params { _player = player})

playerSpeeds :: Lens' PlayerParams (Vector GameSpeed)
playerSpeeds
  = lens _playerSpeeds (\params playerSpeeds ->
                          params { _playerSpeeds = playerSpeeds})

playerUniversals :: Lens' PlayerParams UniversalParams
playerUniversals
  = lens _playerUniversals (\params playerUniversals ->
                          params { _playerUniversals = playerUniversals})

since :: Lens' PlayerParams (Year, MonthOfYear)
since = lens _since (\params since -> params { _since = since})
