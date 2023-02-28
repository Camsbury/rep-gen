{-# LANGUAGE TemplateHaskell #-}
module RepGen.State.Type where

import Foreign.Ptr
import RepGen.PyChess.Type
import RepGen.Action.Type
import RepGen.MoveTree.Type

data RGState
  = RGState
  { _cloudLimitReached :: Bool
  , _chessHelpers      :: Ptr PyObject
  , _moveTree          :: TreeNode
  , _actionStack       :: [RGAction]
  } deriving (Show, Eq)
makeLenses ''RGState
