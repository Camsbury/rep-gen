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


-- | Comparison for MinLoss
minLoss :: Color -> (Uci, TreeNode) -> (Uci, TreeNode) -> Ordering
minLoss c a b = undefined
  where opp = flipColor c


-- | Comparison for MaxWinOverLoss
maxWinOverLoss :: Color -> (Uci, TreeNode) -> (Uci, TreeNode) -> Ordering
maxWinOverLoss = undefined

-- | Get the 'Ordering' needed to fulfill the chosen 'RGStrategy'
strategicCompare
  :: RGStrategy
  -> Color
  -> (Uci, TreeNode)
  -> (Uci, TreeNode)
  -> Ordering
strategicCompare MaxWinOverLoss = maxWinOverLoss
strategicCompare MinLoss        = minLoss

-- | Apply a strategy to select the best move option
applyStrategy :: Vector (Uci, TreeNode) -> RGM Uci
applyStrategy options = do
  sComp <- strategicCompare <$> view strategy <*> view color
  opts <- throwMaybe "No options to apply the strategy to!"
       $ fromNullable options
  let choice = maximumBy sComp opts
  pure $ choice ^. _1
