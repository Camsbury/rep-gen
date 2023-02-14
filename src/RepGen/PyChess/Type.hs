{-# LANGUAGE TemplateHaskell #-}
module RepGen.PyChess.Type where

import Prelude
import RepGen.Type
-- NOTE: makeLenses interrupts python for some reason,
-- so I have to manually make my lenses...
import Control.Lens (Lens', lens)

--------------------------------------------------------------------------------
--- Types
--------------------------------------------------------------------------------

data MateIn
  = MateIn
  { _color     :: Color
  , _moveCount :: Int
  } deriving (Show, Eq)
-- makeFieldsNoPrefix ''MateIn

color :: Lens' MateIn Color
color = lens _color (\person color -> person { _color = color })

moveCount :: Lens' MateIn Int
moveCount = lens _moveCount (\person moveCount -> person { _moveCount = moveCount })

type Cp = Int

data Score
  = CpScore   Cp
  | MateScore MateIn
  deriving (Show, Eq)

data EngineCandidate
  = EngineCandidate
  { _uci   :: !Text
  , _score :: Score
  } deriving (Show, Eq)
-- makeFieldsNoPrefix ''EngineCandidate

uci :: Lens' EngineCandidate Text
uci = lens _uci (\cand uci -> cand { _uci = uci })

score :: Lens' EngineCandidate Score
score = lens _score (\cand score -> cand { _score = score })

