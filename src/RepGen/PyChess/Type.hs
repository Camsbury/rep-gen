{-# LANGUAGE TemplateHaskell #-}
module RepGen.PyChess.Type where

import Prelude
import RepGen.Type
import Control.Lens (makeLenses)

--------------------------------------------------------------------------------
--- Types
--------------------------------------------------------------------------------

data MateIn
  = MateIn
  { _color     :: Color
  , _moveCount :: Int
  } deriving (Show, Eq)
makeLenses ''MateIn

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
makeLenses ''EngineCandidate

