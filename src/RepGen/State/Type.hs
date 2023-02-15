{-# LANGUAGE TemplateHaskell #-}
module RepGen.State.Type where

import Prelude
import Control.Lens (makeLenses)

data RGState
  = RGState
  {
  } deriving (Show, Eq)
makeLenses ''RGState

instance Default RGState where
  def = RGState
