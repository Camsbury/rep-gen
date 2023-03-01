{-# LANGUAGE TemplateHaskell #-}
--------------------------------------------------------------------------------
module RepGen.State.Type where
--------------------------------------------------------------------------------
import Foreign.Ptr
import RepGen.Type
import RepGen.Action.Type
import RepGen.PyChess.Type
import RepGen.MoveTree.Type
import RepGen.Stats.Type
--------------------------------------------------------------------------------
import Data.Aeson
  ( ToJSON(..)
  , object
  )
--------------------------------------------------------------------------------
import qualified Data.Aeson as J
--------------------------------------------------------------------------------

type  PosToInfo = Map Fen PosInfo

data PosInfo
  = PosInfo
  { _posStats     :: RGStats
  , _chosenUci    :: Maybe Uci
  } deriving (Show, Eq)
makeLenses ''PosInfo

instance Default PosInfo where
  def = PosInfo def Nothing

instance ToJSON PosInfo where
  toJSON posInfo =
    object
      [ "posStats"  J..= view posStats posInfo
      , "chosenUci" J..= view chosenUci posInfo
      ]

data RGState
  = RGState
  { _cloudLimitReached :: Bool
  , _posToInfo         :: PosToInfo
  , _chessHelpers      :: Ptr PyObject
  , _moveTree          :: TreeNode
  , _actionStack       :: [RGAction]
  } deriving (Show, Eq)
makeLenses ''RGState
