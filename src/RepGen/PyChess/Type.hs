module RepGen.PyChess.Type where

import Prelude
import RepGen.Type

--------------------------------------------------------------------------------
--- Types
--------------------------------------------------------------------------------

data MateIn
  = MateIn
  { color     :: Color
  , moveCount :: Int
  } deriving (Show, Eq)

type Cp = Int

data Score
  = CpScore   Cp
  | MateScore MateIn
  deriving (Show, Eq)

data EngineCandidate
  = EngineCandidate
  { uci   :: !Text
  , score :: Score
  } deriving (Show, Eq)
