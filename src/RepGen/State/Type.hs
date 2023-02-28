{-# LANGUAGE TemplateHaskell #-}
module RepGen.State.Type where

import Foreign.Ptr
import RepGen.Type
import RepGen.Action.Type
import RepGen.PyChess.Type
import RepGen.MoveTree.Type
import RepGen.Stats.Type

type  PosToInfo = Map Fen PosInfo

data PosInfo
  = PosInfo
  { _posStats     :: RGStats
  , _chosen       :: Bool
  } deriving (Show, Eq)
makeLenses ''PosInfo

instance Default PosInfo where
  def = PosInfo def False

data RGState
  = RGState
  { _cloudLimitReached :: Bool
  , _posToInfo         :: PosToInfo
  , _chessHelpers      :: Ptr PyObject
  , _moveTree          :: TreeNode
  , _actionStack       :: [RGAction]
  } deriving (Show, Eq)
makeLenses ''RGState
