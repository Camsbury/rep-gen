--------------------------------------------------------------------------------
module RepGen.Action.EnumResps
  ( runAction
  , initRunAction
  ) where
--------------------------------------------------------------------------------

import RepGen.Action.Type
import RepGen.Config.Type
import RepGen.Lichess.History
import RepGen.Monad
import RepGen.MoveTree
import RepGen.MoveTree.Type
import RepGen.State.Type
import RepGen.Stats.Type
import RepGen.Type
--------------------------------------------------------------------------------
import qualified RepGen.Engine   as Ngn
import qualified RepGen.MoveTree as MT
import qualified RepGen.Stats    as Stats
import qualified RepGen.State    as State
import qualified RepGen.PyChess  as PyC
--------------------------------------------------------------------------------

-- | Action runner to enumerate responses
runAction :: EnumData -> RGM ()
runAction action = do
  let ucis = action ^. edUcis
  logDebugN $ "Enumerating Responses for: " <> tshow ucis

  processMoves action
  toActOn <- filterMinRespProb
    (action ^. edIsPruned) (action ^. edProbP) (action ^. edProbA) ucis
  let actions = toAction action =<< toActOn
  actionStack %= (actions ++)

-- | Action runner to initiate responses for the tree traversal
initRunAction :: Vector Uci -> RGM ()
initRunAction ucis = do
  logInfoN $ "Initializing Responses for: " <> tshow ucis
  pAgg <- MT.fetchPAgg ucis

  initProcessMoves ucis
  toActOn <- filterMinRespProb True 1 pAgg ucis
  let actions = initToAction =<< toActOn
  actionStack %= (actions ++)

processMoves :: EnumData -> RGM ()
processMoves action = do
  let ucis = action ^. edUcis
  pAgg <- MT.fetchPAgg ucis
  parent
    <- throwMaybe ("Parent doesn't exist at ucis: " <> tshow ucis)
    <=< preuse
    $ moveTree . MT.traverseUcis ucis
  let children = parent ^.. nodeResponses . folded
  when (null children) $ do
    processed <- doProcessMoves action
    let nodes = fromProcessed ucis <$> processed
    moveTree . MT.traverseUcis ucis . nodeResponses .= fromList nodes
    let bestMay = parent ^? bestScoreL . _Just
    let pPrune = action ^. edProbP
    traverse_ (MT.insertNodeInfo True bestMay [] pAgg pPrune ucis) processed

doProcessMoves :: EnumData -> RGM [(Uci, (Double, Fen))]
doProcessMoves action = do
  let ucis = action ^. edUcis
  parent
    <- throwMaybe ("Parent doesn't exist at ucis: " <> tshow ucis)
    <=< preuse
    $ moveTree . MT.traverseUcis ucis
  let pFen    = parent ^. nodeFen

  (rMStats, maybeMastersM) <- maybeMastersMoves pFen
  Stats.updateParentNominal pFen mastersStats rMStats
  (rStats, lichessM) <- lichessMoves pFen
  Stats.updateParentNominal pFen lichessStats rStats

  mOverride <- preview $ overridesL . ix pFen
  processed
    <-   fmap (Ngn.injectEngine [])
    <$> maybe
          (traverse (wrapLCStats ucis) lichessM)
          (mergeMoves ucis lichessM)
          maybeMastersM
  processed' <- pure . fromMaybe processed $ findUci processed =<< mOverride
  traverse_ State.insertChildPosInfo processed'

  pure $ processed' ^.. folded . to (\(u, (f, p, _)) -> (u, (p, f)))


initProcessMoves :: Vector Uci -> RGM ()
initProcessMoves ucis = do
  pAgg <- MT.fetchPAgg ucis
  parent
    <- throwMaybe ("Parent doesn't exist at ucis: " <> tshow ucis)
    <=< preuse
    $ moveTree . MT.traverseUcis ucis
  let children = parent ^.. nodeResponses . folded
  case fromNullable children of
     Just _ ->
       pure ()
     Nothing -> do
       processed <- doInitProcessMoves ucis
       let nodes = fromProcessed ucis <$> processed
       moveTree . MT.traverseUcis ucis . nodeResponses .= fromList nodes
       let bestMay = parent ^? bestScoreL . _Just
       traverse_ (MT.insertNodeInfo True bestMay [] pAgg 1 ucis) processed

-- TODO: DRY up these two functions
doInitProcessMoves :: Vector Uci -> RGM [(Uci, (Double, Fen))]
doInitProcessMoves ucis = do
  parent
    <- throwMaybe ("Parent doesn't exist at ucis: " <> tshow ucis)
    <=< preuse
    $ moveTree . MT.traverseUcis ucis
  let pFen    = parent ^. nodeFen

  (rMStats, maybeMastersM) <- maybeMastersMoves pFen
  Stats.updateParentNominal pFen mastersStats rMStats
  (rStats, lichessM) <- lichessMoves pFen
  Stats.updateParentNominal pFen lichessStats rStats
  engineMoves <- Ngn.fenToEngineCandidates (Just $ length lichessM) pFen
  mOverride <- preview $ overridesL . ix pFen
  processed
    <-   fmap (Ngn.injectEngine engineMoves)
    <$> maybe
          (traverse (wrapLCStats ucis) lichessM)
          (mergeMoves ucis lichessM)
          maybeMastersM
  processed' <- pure . fromMaybe processed $ findUci processed =<< mOverride
  traverse_ State.insertChildPosInfo processed'

  pure $ processed' ^.. folded . to (\(u, (f, p, _)) -> (u, (p, f)))

fromProcessed :: Vector Uci -> (Uci, (Double, Fen)) -> (Uci, TreeNode)
fromProcessed ucis (uci, (prob, fen))
  = ( uci
    , def & uciPath   .~ snoc ucis uci
          & nodeFen   .~ fen
          & probLocal .~ prob
    )

findUci :: [(Uci, (Fen, Double, PosInfo))] -> Uci -> Maybe [(Uci, (Fen, Double, PosInfo))]
findUci cands uci
  = fmap toList
  . fromNullable
  $ cands ^.. folded . filtered (\x -> x ^. _1 == uci)

mergeMoves
  :: Vector Uci
  -> [(Uci, (Double, NodeStats))]
  -> [(Uci, NodeStats)]
  -> RGM [(Uci, (Fen, Double, PosInfo))]
mergeMoves ucis lichessM mastersM
  = traverse f lichessM
  where
    f (uci, (prob, lcm)) = do
      pModule <- use chessHelpers
      fen <- liftIO . PyC.ucisToFen pModule $ snoc ucis uci
      pure
        ( uci
        , ( fen
          , prob
          , def & posStats .~ RGStats
            { _lichessStats = Just lcm
            , _mastersStats = lookup uci mastersM
            , _rgScore      = Nothing
            }
          )
        )

wrapLCStats
  :: Vector Uci
  -> (Uci, (Double, NodeStats))
  -> RGM (Uci, (Fen, Double, PosInfo))
wrapLCStats ucis (uci, (prob, lcm))
  = do
    pModule <- use chessHelpers
    fen <- liftIO . PyC.ucisToFen pModule $ snoc ucis uci
    pure
      ( uci
      , ( fen
        , prob
        , def & posStats .~ RGStats
          { _lichessStats = Just lcm
          , _mastersStats = Nothing
          , _rgScore      = Nothing
          }
        )
      )

-- | Turn viable responses into actions
toAction :: EnumData -> (Double, Double, TreeNode) -> [RGAction]
toAction action (pPrune, _, node)
  = [ RGAEnumCands
      $ EnumData
      { _edUcis     = node ^. uciPath
      , _edProbP    = pPrune
      , _edProbA    = action ^. edProbA
      , _edDepth    = action ^. edDepth
      , _edIsPruned = action ^. edIsPruned
      }
    , RGATransStats $ node ^. uciPath
    ]

-- | Turn viable responses into actions
initToAction ::  (Double, Double, TreeNode) -> [RGAction]
initToAction (pPrune, pAgg, node)
  = [ RGAEnumCands
      $ EnumData
      { _edUcis = node ^. uciPath
      , _edProbP = pPrune
      , _edProbA = pAgg
      , _edDepth = 1
      , _edIsPruned = False
      }
    , RGAPruneCands $ node ^. uciPath
    , RGATransStats $ node ^. uciPath
    ]

-- | Filter responses to act on by minimum response probability
filterMinRespProb
  :: Bool
  -> Double
  -> Double
  -> Vector Uci
  -> RGM [(Double, Double, TreeNode)]
filterMinRespProb isPruned pPrune pAggPrune ucis = do
  irp <- view initRespProb
  arp <- view asymRespProb
  mpa <- view minProbAgg
  pAgg <- MT.fetchPAgg ucis
  -- let minProb = arp + (irp - arp) * pAggPrune
  let minProb = exp (log (irp / arp) * pAggPrune) * arp
  logDebugN $ "filtering for ucis: " <> tshow ucis
  logDebugN $ "pAgg: " <> tshow pAgg
  logDebugN $ "pAggPrune: " <> tshow pAggPrune
  logDebugN $ "minProb: " <> tshow minProb
  -- traverse_
  --   (\rNode ->
  --     logInfoN $ "nodeProbPrune: " <> tshow
  --       ((pPrune *) <$>
  --       (pTI ^? ixPTI (rNode ^. _2 . nodeFen) . posStats . lichessStats . _Just . prob))
  --   )
  --   =<< use (moveTree . traverseUcis ucis . nodeResponses)
  let isValid rNode =
          (\x -> (isPruned && mpa < (pAgg * x)) || pPrune * x > minProb)
          (rNode ^. probLocal)

  when isPruned $
    moveTree
      . traverseUcis ucis
      . nodeResponses
      . traversed
      . filtered (\x -> x ^. _2 . to (not . isValid))
      . _2
      . removed
      .= True
  children
    <- use
    $ moveTree
    . traverseUcis ucis
    . to (collectFilteredChildren (\x -> x ^. _2 . to isValid))
  traverse addProbs $ children ^.. folded . _2
  where
    addProbs :: TreeNode -> RGM (Double, Double, TreeNode)
    addProbs child = do
      let cProb = child ^. probLocal
      pure (pPrune * cProb, pAggPrune * cProb, child)



