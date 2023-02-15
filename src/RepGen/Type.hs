{-# LANGUAGE TemplateHaskell #-}
module RepGen.Type where

import Prelude
import Control.Lens (makeLenses)

--------------------------------------------------------------------------------
--- Types
--------------------------------------------------------------------------------

data Color
  = White
  | Black
  deriving (Show, Eq)

type RGError = Text

type San = Text
type Uci = Text

data RGStrategy
  = MinLoss
  | MaxWinOverLoss
  deriving (Show, Eq)

data RGState
  = RGState
  {
  } deriving (Show, Eq)
makeLenses ''RGState
