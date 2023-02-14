{-# LANGUAGE TemplateHaskell #-}
module RepGen.PyChess.Type where

import Prelude
import RepGen.Type
import Control.Lens (makeFieldsNoPrefix)

--------------------------------------------------------------------------------
--- Types
--------------------------------------------------------------------------------

data MateIn
  = MateIn
  { _color     :: Color
  , _moveCount :: Int
  } deriving (Show, Eq)
makeFieldsNoPrefix ''MateIn

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
makeFieldsNoPrefix ''EngineCandidate
