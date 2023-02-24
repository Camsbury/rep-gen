{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
--------------------------------------------------------------------------------
module RepGen.Strategy.Type where
--------------------------------------------------------------------------------

data RGOptimizer
  = MinLoss
  | MaxWinOverLoss
  deriving (Show, Eq)
makePrisms ''RGOptimizer

instance Default RGOptimizer where
  def = MaxWinOverLoss

data EngineFilter = EngineFilter
  { _engineAllowableLoss :: Double
  , _engineMoveCount :: Int
  , _engineDepth :: Int
  } deriving (Show, Eq)
makeLenses ''EngineFilter

instance Default EngineFilter where
  def = EngineFilter 0.9 10 20

data RGSatisficers = RGSatisficers
  { _engineFilter :: EngineFilter
  } deriving (Show, Eq)
makeLenses ''RGSatisficers

instance Default RGSatisficers where
  def = RGSatisficers def

-- | The strategy for the repertoire generator.
-- There is one optimizer and any number of satisficers used.
data RGStrategy = RGStrategy
  { _optimizer   :: RGOptimizer
  , _satisficers :: RGSatisficers
  } deriving (Show, Eq)
makeLenses ''RGStrategy

instance Default RGStrategy where
  def = RGStrategy def def
