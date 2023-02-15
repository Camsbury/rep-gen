{-# LANGUAGE TemplateHaskell #-}
module RepGen.State.Type where

import Prelude
import Control.Lens (makeLenses)

import RepGen.Action.Type

data RGState
  = RGState
  { _tree :: ()
  , _actionStack :: Vector RGAction
  } deriving (Show, Eq)
makeLenses ''RGState

instance Default RGState where
  def = RGState () empty
