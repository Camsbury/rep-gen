--------------------------------------------------------------------------------
module RepGen.Action.PruneCands
  ( runAction
  ) where
--------------------------------------------------------------------------------
import RepGen.Action.Type
import RepGen.Monad
import RepGen.Type
import RepGen.MoveTree
import RepGen.MoveTree.Type
import RepGen.State.Type
--------------------------------------------------------------------------------
import qualified RepGen.Strategy as Strat
import qualified RepGen.Export as X
--------------------------------------------------------------------------------
runAction :: Vector Uci -> RGM ()
runAction ucis = do
  let err = "Node to prune does not exist at: " <> intercalate "," ucis
  node <- throwMaybe err <=< preuse $ moveTree . traverseUcis ucis
  let children = node ^.. responses . folded
  (choiceUci, _) <- Strat.applyStrategy children
  -- "remove" the others
  moveTree
    . traverseUcis ucis
    . responses
    . traversed
    . filtered (\x -> x ^. _1 /= choiceUci)
    . _2
    . removed
    .= True

  let newUcis = snoc ucis choiceUci

  actionStack %= (toActions newUcis ++)

  X.exportTree

  logInfoN
    $ "The tree has been pruned to: "
    <> intercalate "," newUcis

toActions :: Vector Uci -> [RGAction]
toActions ucis
  = [ RGAEnumResps $ EnumData ucis 1 1 True
    , RGAPruneHooks ucis
    , RGACalcStats ucis
    ]
