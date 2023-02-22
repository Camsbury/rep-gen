{-# LANGUAGE TemplateHaskell #-}
module RepGen.State.Type where



import RepGen.Action.Type
import RepGen.MoveTree.Type

data RGState
  = RGState
  { _cloudLimitReached :: Bool
  , _moveTree          :: TreeNode
  , _actionStack       :: [RGAction]
  } deriving (Show, Eq)
makeLenses ''RGState

instance Default RGState where
  def = RGState False def empty
