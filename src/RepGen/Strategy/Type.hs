{-# LANGUAGE TemplateHaskell #-}
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

data RGSatisficer
  = RGEngineFilter
  deriving (Show, Eq)
makePrisms ''RGSatisficer

instance Default RGSatisficer where
  def = RGEngineFilter

-- | The strategy for the repertoire generator.
-- There is one optimizer and any number of satisficers used.
data RGStrategy = RGStrategy
  { _optimizer   :: RGOptimizer
  , _satisficers :: [RGSatisficer]
  } deriving (Show, Eq)
makeLenses ''RGStrategy

instance Default RGStrategy where
  def = RGStrategy def def
