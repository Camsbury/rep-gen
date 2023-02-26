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
  logInfoN $ "Pruning Candidates for: " <> tshow ucis
  let err = "Node to prune does not exist at: " <> tshow ucis
  node <- throwMaybe err <=< preuse $ moveTree . traverseUcis ucis
  let children = node ^.. responses . folded
  -- logDebugN $ "Prune Children: " <> tshow children
  logInfoN
    $ ("Candidate Moves: " <>)
    . tshow
    $ children ^.. folded . _1

  case fromNullable children of
    Nothing
      -> logWarnN
      $ "No children when pruning at: "
      <> tshow ucis
    Just _ -> do
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

      let actions = toActions newUcis
      -- logDebugN $ "Actions: " <> tshow actions

      actionStack %= (actions ++)

      X.exportJSON

      logInfoN
        $ "The tree has been pruned to: "
        <> tshow newUcis

toActions :: Vector Uci -> [RGAction]
toActions ucis
  = [ RGAEnumResps $ EnumData ucis 1 1 True
    , RGAPruneHooks ucis
    , RGACalcStats ucis
    ]
