{-# LANGUAGE TemplateHaskell #-}
module RepGen.Action.Type where


import RepGen.Type

data CalcStats
  = CalcStats
  { _calcUcis :: Vector Uci
  } deriving (Show, Eq)
makeLenses ''CalcStats

data TransStats
  = TransStats
  { _transUcis :: Vector Uci
  } deriving (Show, Eq)
makeLenses ''TransStats

data EnumCands
  = EnumCands
  {
  } deriving (Show, Eq)
makeLenses ''EnumCands

data EnumResps
  = EnumResps
  {
  } deriving (Show, Eq)
makeLenses ''EnumResps

data PruneCands
  = PruneCands
  {
  } deriving (Show, Eq)
makeLenses ''PruneCands

data PruneHooks
  = PruneHooks
  {
  } deriving (Show, Eq)
makeLenses ''PruneHooks

data RGAction
  = RGACalcStats  CalcStats
  | RGATransStats TransStats
  | RGAEnumCands  EnumCands
  | RGAEnumResps  EnumResps
  | RGAPruneCands PruneCands
  | RGAPruneHooks PruneHooks
  deriving (Show, Eq)
makePrisms ''RGAction
