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
  let depth = succ $ action ^. edDepth
  fen <- liftIO $ PyC.ucisToFen ucis
  eDepth <- view $ engineConfig . engineDepth
  eMC <- view $ engineConfig . engineMoveCount
  lcM <- H.lichessMoves fen
  maybeMM <- H.maybeMastersMoves fen
  let candidates = fromMaybe lcM maybeMM
  engineMoves <- Ngn.fenToEngineCandidates fen eDepth eMC
  maybeOverride <- preview $ overridesL . ix fen
  maybeMatch <- findUci candidates fen maybeOverride
  filteredCands
    <- maybe (filterCandidates candidates engineMoves) pure maybeMatch
  let isMasters = isJust maybeMM
  pAgg <- MT.fetchPAgg ucis
  let pPrune = action ^. edProbP
  let candNodes = initNode isMasters pAgg pPrune fen ucis <$> filteredCands
  let candNodes' = if isMasters then injectLichess lcM <$> candNodes else candNodes
  let candNodes'' = injectEngine engineMoves <$> candNodes'
  candNodes''' <- if null candNodes'' then firstEngine engineMoves else pure candNodes''
  moveTree . MT.traverseUcis ucis . responses .= fromList candNodes'''
  toActOn <- undefined
  actionStack %= ((toAction action =<< reverse toActOn) ++)

toAction :: EnumData -> TreeNode -> [RGAction]
toAction action node = undefined

-- TODO: log in here that this happened
firstEngine :: [EngineCandidate] -> RGM [(Uci, TreeNode)]
firstEngine = undefined

injectEngine :: [EngineCandidate] -> (Uci, TreeNode) -> (Uci, TreeNode)
injectEngine nCands stats@(uci, _)
  = stats
  & _2
  . rgStats
  . score
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

