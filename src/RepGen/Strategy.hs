--------------------------------------------------------------------------------
module RepGen.Strategy
  ( module RepGen.Strategy
  ) where
--------------------------------------------------------------------------------
import Prelude
import RepGen.Config.Type
import RepGen.Monad
import RepGen.MoveTree.Type
import RepGen.Strategy.Type
import RepGen.Type
--------------------------------------------------------------------------------

-- | Get the 'Ordering' needed to fulfill the chosen 'RGStrategy'
strategyToOrdering
  :: RGStrategy
  -> (Uci, TreeNode)
  -> (Uci, TreeNode)
  -> Ordering
strategyToOrdering = undefined

-- | Apply a strategy to select the best move option
applyStrategy :: Vector (Uci, TreeNode) -> RGM Uci
applyStrategy options = do
  strat <- view strategy
  opts <- throwMaybe "No options to apply the strategy to!"
       $ fromNullable options
  let choice = maximumBy (strategyToOrdering strat) opts
  pure $ choice ^. _1
