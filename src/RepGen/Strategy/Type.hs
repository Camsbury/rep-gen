--------------------------------------------------------------------------------
module RepGen.Strategy.Type
  ( module RepGen.Strategy.Type
  ) where
--------------------------------------------------------------------------------
import Prelude
--------------------------------------------------------------------------------

data RGStrategy
  = MinLoss
  | MaxWinOverLoss
  deriving (Show, Eq)
