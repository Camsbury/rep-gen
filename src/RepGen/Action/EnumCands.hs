--------------------------------------------------------------------------------
module RepGen.Action.EnumCands
  ( runAction
  ) where
--------------------------------------------------------------------------------

import RepGen.Action.Type
import RepGen.Config.Type
import RepGen.Engine.Type
import RepGen.Monad
import RepGen.MoveTree.Type
import RepGen.Score.Type
import RepGen.State.Type
import RepGen.Stats.Type
import RepGen.Type
--------------------------------------------------------------------------------
import qualified RepGen.PyChess         as PyC
import qualified RepGen.Engine          as Ngn
import qualified RepGen.MoveTree        as MT
import qualified RepGen.Lichess.History as H
--------------------------------------------------------------------------------

-- | Action runner to enumerate move candidates
runAction :: EnumData -> RGM ()
runAction action = do
  let ucis = action ^. edUcis
  candidates <- fetchCandidates action
  moveTree . MT.traverseUcis ucis . responses .= fromList candidates
  sDepth <- view searchDepth
  let addActions
        = action ^. edIsPruned && length candidates == 1
        || action ^. edDepth == sDepth
  when addActions
    $ actionStack
    %= ((toAction action =<< reverse (candidates ^.. folded . _2)) ++)

fetchCandidates :: EnumData -> RGM [(Uci, TreeNode)]
fetchCandidates action = do
  let ucis       = action ^. edUcis
      pPrune     = action ^. edProbP
  fen            <- liftIO $ PyC.ucisToFen ucis
  lcM            <- H.lichessMoves fen
  maybeMM        <- H.maybeMastersMoves fen
  engineMoves    <- Ngn.fenToEngineCandidates fen
  maybeOverride  <- preview $ overridesL . ix fen
  let candidates = fromMaybe lcM maybeMM
      isMasters  = isJust maybeMM

  maybeMatch <- findUci candidates fen maybeOverride
  filteredCands
    <- maybe (filterCandidates candidates engineMoves) pure maybeMatch
  pAgg <- MT.fetchPAgg ucis

  let candNodes = initNode isMasters pAgg pPrune fen ucis <$> filteredCands
  let candNodes' = if isMasters then injectLichess lcM <$> candNodes else candNodes
  let candNodes'' = injectEngine engineMoves <$> candNodes'
  if null candNodes'' then firstEngine pPrune fen ucis engineMoves else pure candNodes''

toAction :: EnumData -> TreeNode -> [RGAction]
toAction (EnumData _ probP depth _) node
  = [ RGAEnumResps $ EnumData ucis probP (succ depth) False
    , RGACalcStats ucis
    ]
  where
    ucis = node ^. uciPath

firstEngine
  :: Double
  -> Fen
  -> Vector Uci
  -> [EngineCandidate]
  -> RGM [(Uci, TreeNode)]
firstEngine pPrune fen ucis (ngn:_) = do
  pAgg <- MT.fetchPAgg ucis
  logInfoN $ "There are no candidates for FEN: " <> fen ^. fenL
  logInfoN "Reverting to the top engine move"
  let uci = ngn ^. ngnUci
  pure
    [( uci
     , def & rgStats .~ mkRGStats pPrune pAgg
           & uciPath .~ snoc ucis uci
           & nodeFen .~ fen
     )]
firstEngine _ fen _ [] = do
  logInfoN $ "There are no candidates for FEN: " <> fen ^. fenL
  logInfoN "Nor are there any engine moves."
  pure []

injectEngine :: [EngineCandidate] -> (Uci, TreeNode) -> (Uci, TreeNode)
injectEngine nCands stats@(uci, _)
  = stats
  & _2
  . rgStats
  . rgScore
  .~ findBy uci nCands
  where
    findBy _ [] = Nothing
    findBy u (ngn:rest)
      | u == ngn ^. ngnUci = Just . mkRGStat $ ngn ^. ngnScore . scoreL
      | otherwise = findBy u rest

injectLichess :: [(Uci, NodeStats)] -> (Uci, TreeNode) -> (Uci, TreeNode)
injectLichess lcM stats@(uci, _)
  = stats
  & _2
  . rgStats
  . lichessStats
  .~ lookup uci lcM

initNode
  :: Bool
  -> Double
  -> Double
  -> Fen
  -> Vector Uci
  -> (Uci, NodeStats)
  -> (Uci, TreeNode)
initNode isMasters pAgg pPrune fen ucis (uci, ns)
  = (uci,)
  $ def
  & uciPath .~ snoc ucis uci
  & nodeFen .~ fen
  & rgStats .~ stats
  where
    nsL = if isMasters then mastersStats else lichessStats
    stats
      = def
      & nsL       ?~ ns
      & probPrune .~  pPrune
      & probAgg   .~  pAgg

findUci
  :: [(Uci, NodeStats)]
  -> Fen
  -> Maybe Uci
  -> RGM (Maybe [(Uci, NodeStats)])
findUci _ _ Nothing = pure Nothing
findUci cands fen (Just u)
  = maybe logMiss (pure . Just . (:[]))
  $ cands ^? traversed . filtered (matchesUci u)
  where
    logMiss
      = logInfoN ("No data for override: " <> u <> " at FEN: " <> (fen ^. fenL))
      -- NOTE: could rewrite to store the Uci with no NodeStats
      -- , but skipping for now
      >> pure Nothing
    matchesUci u' (x, _) = x == u'

-- | Filter available candidates for selection
filterCandidates
  :: [(Uci, NodeStats)]
  -> [EngineCandidate]
  -> RGM [(Uci, NodeStats)]
filterCandidates mvs eCands = do
  let eUcis = eCands ^.. folded . ngnUci
  mpl <- view minPlays
  pure . filter (g eUcis) $ filter (f mpl) mvs
  where
    f mpl (_, s) = mpl < s ^. playCount
    g eUcis (u, _) = u `elem` eUcis

