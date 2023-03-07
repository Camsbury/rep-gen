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
import qualified RepGen.State    as State
import qualified RepGen.Export as X
--------------------------------------------------------------------------------
runAction :: Vector Uci -> RGM ()
runAction ucis = do
  logInfoN $ "Pruning Candidates for: " <> tshow ucis
  let err = "Node to prune does not exist at: " <> tshow ucis
  node <- throwMaybe err <=< preuse $ moveTree . traverseUcis ucis
  let fen = node ^. nodeFen

  maybeChosenUci
    <- use
    $ posToInfo
    . ix fen
    . chosenUci

  case maybeChosenUci of
    -- Transposes to another chosen child, no need to repeat work.
    Just choiceUci -> do
      moveTree
        . traverseUcis ucis
        . nodeResponses
        . traversed
        . filtered (\x -> x ^. _1 /= choiceUci)
        . _2
        . removed
        .= True
      moveTree
        . traverseUcis ucis
        . nodeResponses
        . traversed
        . filtered (\x -> x ^. _1 == choiceUci)
        . _2
        . transposes
        .= True
      X.exportJSON

      -- no need to create actions for transposition

      let newUcis = snoc ucis choiceUci
      logInfoN
        $ "The tree has been pruned to: "
        <> tshow newUcis

    Nothing -> do
      let children = node ^.. nodeResponses . folded
      -- logDebugN $ "Prune Children: " <> tshow children
      logInfoN
        $ ("Candidate Moves: " <>)
        . tshow
        $ children ^.. folded . _1

      childrenStats <- traverse State.collectInfo children

      case fromNullable childrenStats of
        Nothing
          -> logWarnN
          $ "No children when pruning at: "
          <> tshow ucis
        Just _ -> do
          appliedStrat <- Strat.applyStrategy childrenStats
          case appliedStrat of
            Just (choiceUci, _) -> do
              -- "remove" the others
              moveTree
                . traverseUcis ucis
                . nodeResponses
                . traversed
                . filtered (\x -> x ^. _1 /= choiceUci)
                . _2
                . removed
                .= True

              -- add chosen uci as possible transposition
              posToInfo . ix fen . chosenUci ?= choiceUci

              let newUcis = snoc ucis choiceUci

              let actions = toActions newUcis
              -- logDebugN $ "Actions: " <> tshow actions

              actionStack %= (actions ++)

              X.exportJSON

              logInfoN
                $ "The tree has been pruned to: "
                <> tshow newUcis
            Nothing
              -> logDebugN
              $ "No sound candidates when pruning at: "
              <> tshow ucis

toActions :: Vector Uci -> [RGAction]
toActions ucis
  = [ RGAEnumResps $ EnumData ucis 1 1 True
    , RGAPruneHooks ucis
    , RGACalcStats ucis
    ]
