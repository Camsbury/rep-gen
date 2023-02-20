{-# LANGUAGE TemplateHaskell #-}
--------------------------------------------------------------------------------
module RepGen.Score.Type
  ( module RepGen.Score.Type
  ) where
--------------------------------------------------------------------------------
import RepGen.Type
--------------------------------------------------------------------------------

type Cp = Int

data MateIn
  = MateIn
  { _miColor     :: Color
  , _moveCount :: Int
  } deriving (Show, Eq)
makeLenses ''MateIn

data RawScore
  = CpScore   Cp
  | MateScore MateIn
  deriving (Show, Eq)

newtype Score
  = Score
  { _scoreL :: Double
  } deriving (Show, Eq, Ord)
makeLenses ''Score
