{-# LANGUAGE TemplateHaskell #-}
--------------------------------------------------------------------------------
module RepGen.Action.Type where
--------------------------------------------------------------------------------
import RepGen.Type
--------------------------------------------------------------------------------

data EnumData
  = EnumData
  { _edUcis     :: Vector Uci
  , _edProb     :: Double
  , _edDepth    :: Int
  , _edIsPruned :: Bool
  } deriving (Show, Eq)
makeLenses ''EnumData

data RGAction
  = RGACalcStats  (Vector Uci)
  | RGATransStats (Vector Uci)
  | RGAEnumCands  EnumData
  | RGAEnumResps  EnumData
  | RGAInitResps  (Vector Uci)
  | RGAPruneCands (Vector Uci)
  | RGAPruneHooks (Vector Uci)
  deriving (Show, Eq)
makePrisms ''RGAction
