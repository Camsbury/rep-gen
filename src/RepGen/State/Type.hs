{-# LANGUAGE TemplateHaskell #-}
module RepGen.State.Type where



import RepGen.Action.Type
import RepGen.MoveTree.Type

data RGState
  = RGState
  { _moveTree :: TreeNode
  , _actionStack :: Vector RGAction
  } deriving (Show, Eq)
makeLenses ''RGState

instance Default RGState where
  def = RGState def empty
