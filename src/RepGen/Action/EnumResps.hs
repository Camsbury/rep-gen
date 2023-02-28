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
  pAgg <- MT.fetchPAgg ucis

  processMoves action
  toActOn <- filterMinRespProb
    (action ^. edIsPruned) (action ^. edProbP) pAgg ucis
  let actions = toAction action =<< toActOn
  actionStack %= (actions ++)

-- | Action runner to initiate responses for the tree traversal
initRunAction :: Vector Uci -> RGM ()
initRunAction ucis = do
  logInfoN $ "Initializing Responses for: " <> tshow ucis
  pAgg <- MT.fetchPAgg ucis

  initProcessMoves ucis
  toActOn <- filterMinRespProb False 1 pAgg ucis
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
  case fromNullable children of
     Just _ ->
       pure ()
     Nothing -> do
       processed <- doProcessMoves action pAgg
       let nodes = fromProcessed ucis <$> processed
       moveTree . MT.traverseUcis ucis . nodeResponses .= fromList nodes

doProcessMoves :: EnumData -> Double -> RGM [(Uci, Fen)]
doProcessMoves action pAgg = do
  let ucis = action ^. edUcis
  parent
    <- throwMaybe ("Parent doesn't exist at ucis: " <> tshow ucis)
    <=< preuse
    $ moveTree . MT.traverseUcis ucis
  let pFen = parent ^. nodeFen

  (rMStats, maybeMastersM) <- maybeMastersMoves pFen
  Stats.updateParentNominal pFen mastersStats rMStats
  (rStats, lichessM') <- lichessMoves pFen
  Stats.updateParentNominal pFen lichessStats rStats

  lichessM <- filterMoves lichessM'
  engineMoves <- Ngn.fenToEngineCandidates pFen
  mOverride <- preview $ overridesL . ix pFen
  let pPrune = action ^. edProbP
  processed
    <-   fmap (Ngn.injectEngine engineMoves)
    <$> maybe
          (traverse (wrapLCStats ucis pAgg pPrune) lichessM)
          (mergeMoves ucis pAgg pPrune lichessM)
          maybeMastersM
  processed' <- pure . fromMaybe processed $ findUci processed =<< mOverride
  traverse_ State.insertChildPosInfo processed'
  pure $ processed' ^.. folded . to (\(u, (f, _)) -> (u, f))


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
       processed <- doInitProcessMoves ucis pAgg
       let nodes = fromProcessed ucis <$> processed
       moveTree . MT.traverseUcis ucis . nodeResponses .= fromList nodes

-- TODO: DRY up these two functions
doInitProcessMoves :: Vector Uci -> Double -> RGM [(Uci, Fen)]
doInitProcessMoves ucis pAgg = do
  parent
    <- throwMaybe ("Parent doesn't exist at ucis: " <> tshow ucis)
    <=< preuse
    $ moveTree . MT.traverseUcis ucis
  let pFen = parent ^. nodeFen

  (rMStats, maybeMastersM) <- maybeMastersMoves pFen
  Stats.updateParentNominal pFen mastersStats rMStats
  (rStats, lichessM) <- lichessMoves pFen
  Stats.updateParentNominal pFen lichessStats rStats
  engineMoves <- Ngn.fenToEngineCandidates pFen
  mOverride <- preview $ overridesL . ix pFen
  processed
    <-   fmap (Ngn.injectEngine engineMoves)
    <$> maybe
          (traverse (wrapLCStats ucis pAgg 1) lichessM)
          (mergeMoves ucis pAgg 1 lichessM)
          maybeMastersM
  processed' <- pure . fromMaybe processed $ findUci processed =<< mOverride
  traverse_ State.insertChildPosInfo processed'
  pure $ processed' ^.. folded . to (\(u, (f, _)) -> (u, f))

fromProcessed :: Vector Uci -> (Uci, Fen) -> (Uci, TreeNode)
fromProcessed ucis (uci, fen)
  = ( uci
    , TreeNode (snoc ucis uci) fen mempty False
    )

findUci :: [(Uci, (Fen, PosInfo))] -> Uci -> Maybe [(Uci, (Fen, PosInfo))]
findUci cands uci
  = fmap toList
  . fromNullable
  $ cands ^.. folded . filtered (\x -> x ^. _1 == uci)


filterMoves
  :: [(Uci, NodeStats)]
  -> RGM [(Uci, NodeStats)]
filterMoves mvs = do
  mpl <- view minPlays
  pure $ filter (f mpl) mvs
  where
    f mpl (_, s) = mpl < s ^. playCount

mergeMoves
  :: Vector Uci
  -> Double
  -> Double
  -> [(Uci, NodeStats)]
  -> [(Uci, NodeStats)]
  -> RGM [(Uci, (Fen, PosInfo))]
mergeMoves ucis pAgg pPrune lichessM mastersM
  = traverse f lichessM
  where
    f (uci, lcm) = do
      pModule <- use chessHelpers
      fen <- liftIO . PyC.ucisToFen pModule $ snoc ucis uci
      pure
        ( uci
        , ( fen
          , def & posStats .~ RGStats
            { _lichessStats = Just lcm
            , _mastersStats = lookup uci mastersM
            , _rgScore      = Nothing
            , _probPrune    = pPrune * lcm ^. prob
            , _probAgg      = pAgg * lcm ^. prob
            }
          )
        )

wrapLCStats
  :: Vector Uci
  -> Double
  -> Double
  -> (Uci, NodeStats)
  -> RGM (Uci, (Fen, PosInfo))
wrapLCStats ucis pAgg pPrune (uci, lcm)
  = do
    pModule <- use chessHelpers
    fen <- liftIO . PyC.ucisToFen pModule $ snoc ucis uci
    pure
      ( uci
      , ( fen
        , def & posStats .~ RGStats
          { _lichessStats = Just lcm
          , _mastersStats = Nothing
          , _rgScore      = Nothing
          , _probPrune    = pPrune * lcm ^. prob
          , _probAgg      = pAgg * lcm ^. prob
          }
        )
      )

-- | Turn viable responses into actions
toAction :: EnumData -> (Double, TreeNode) -> [RGAction]
toAction action (pPrune, node)
  = [ RGAEnumCands
      $ EnumData
      { _edUcis = node ^. uciPath
      , _edProbP = pPrune
      , _edDepth = action ^. edDepth
      , _edIsPruned = action ^. edIsPruned
      }
    , RGATransStats $ node ^. uciPath
    ]

-- | Turn viable responses into actions
initToAction ::  (Double, TreeNode) -> [RGAction]
initToAction (pPrune, node)
  = [ RGAEnumCands
      $ EnumData
      { _edUcis = node ^. uciPath
      , _edProbP = pPrune
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
  -> RGM [(Double, TreeNode)]
filterMinRespProb isPruned pPrune pAgg ucis = do
  irp <- view initRespProb
  arp <- view asymRespProb
  mpa <- view minProbAgg
  pTI <- use posToInfo
  let minProb = exp (log (irp / arp) * pAgg) * arp
  let isValid rNode =
        maybe
          False
          (\x -> (isPruned && mpa < (pAgg * x)) || pPrune * x > minProb)
          (pTI ^? ix (rNode ^. nodeFen) . posStats . lichessStats . _Just . prob)
  moveTree
    . traverseUcis ucis
    . nodeResponses
    . traversed
    . filtered (\x -> x ^. _2 . to (not . isValid))
    . _2
    . removed
    .= True
  children <- use $ moveTree . traverseUcis ucis . to collectValidChildren
  traverse addPPrune $ children ^.. folded . _2
  where
    addPPrune :: TreeNode -> RGM (Double, TreeNode)
    addPPrune child = do
      let fen = child ^. nodeFen
      cPPrune
        <- throwMaybe ("No pos info exists for ucis: " <> tshow (child ^. uciPath))
        <=< preuse
        $ posToInfo . ix fen . posStats . lichessStats . _Just . prob . to (* pPrune)
      pure (cPPrune, child)



