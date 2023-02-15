{-# LANGUAGE TemplateHaskell #-}
module RepGen.State.Type where

import Prelude
import Control.Lens (makeLenses)
import Data.Default

data RGState
  = RGState
  {
  } deriving (Show, Eq)
makeLenses ''RGState

instance Default RGState where
  def = RGState
