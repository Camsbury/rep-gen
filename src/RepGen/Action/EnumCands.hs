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
import RepGen.State.Type
import RepGen.Stats.Type
import RepGen.Strategy.Type
import RepGen.Type
--------------------------------------------------------------------------------
import qualified RepGen.PyChess         as PyC
import qualified RepGen.Engine          as Ngn
import qualified RepGen.MoveTree        as MT
import qualified RepGen.Lichess.History as H
import qualified RepGen.Strategy        as Strat
import qualified RepGen.Stats           as Stats
import qualified RepGen.State           as State
--------------------------------------------------------------------------------

-- | Action runner to enumerate move candidates
runAction :: EnumData -> RGM ()
runAction action = do
  let ucis = action ^. edUcis
  logInfoN $ "Candidates for: " <> tshow ucis
  candidates <- fetchCandidates action
  traverse_ (logInfoN . ("  - " <>) . tshow) $ candidates ^.. folded . _1
  sDepth <- view searchDepth
  pTI <- use posToInfo
  -- used to stop eval if scores are super high
  let maybeBestScore
        = maximumMay
        $ candidates ^.. folded . _2 . _2 . to (\f -> pTI ^? ixPTI f . posStats . rgScore . _Just . nom) . _Just
  -- if this is the final enumeration and there is nothing, remove the parent
  when (action ^. edIsPruned && null candidates)
    $ moveTree . MT.traverseUcis ucis . removed .= True

  let addActions
        = (not (action ^. edIsPruned) || length candidates /= 1)
        && action ^. edDepth /= sDepth
        && maybe True (< scoreCap) maybeBestScore
  let actions = toAction action =<< (candidates ^.. folded . _1 . to (snoc ucis))
  when addActions $ actionStack %= (actions ++)

-- | This is so we don't go searching beyond mate and breaking stupid python code
scoreCap :: Double
scoreCap = 0.9

fetchCandidates :: EnumData -> RGM [(Uci, (Double, Fen))]
fetchCandidates action = do
  let ucis = action ^. edUcis
  parent
    <- throwMaybe ("Parent doesn't exist at ucis: " <> tshow ucis)
    <=< preuse
    $ moveTree . MT.traverseUcis ucis
  let children = parent ^.. nodeResponses . folded
  case fromNullable children of
     Just _ -> do
       logDebugN $ "Candidates already exist for ucis: " <> tshow ucis
       pure $ children ^.. folded . to (\(u, n) -> (u, (n ^. probLocal, n ^. nodeFen)))
     Nothing -> do
       let isPruned = action ^. edIsPruned
       let pFen     = parent ^. nodeFen
       pAgg         <- MT.fetchPAgg ucis
       breadth      <- maxCandBreadth isPruned pAgg
       engineMoves  <- Ngn.fenToEngineCandidates (Just $ breadth + ngnBuffer) pFen
       candidates   <- doFetchCandidates action engineMoves
       let nodes    = fromProcessed ucis <$> candidates
       let pPrune   = action ^. edProbP
       let bestMay  = parent ^? bestScoreL . _Just

       -- create nodes
       moveTree . MT.traverseUcis ucis . nodeResponses .= fromList nodes

       -- add stats nodes
       traverse_ (MT.insertNodeInfo False bestMay engineMoves pAgg pPrune ucis) candidates
       pure candidates

-- | Added to the candidate breadth to ensure enough data exists for our choices
-- NOTE: could write an engine function that takes a position and a list of moves and returns the eval for each (shouldn't take much time since the breadth would just be 1 for each)
ngnBuffer :: Int
ngnBuffer = 5

doFetchCandidates :: EnumData -> [EngineCandidate] -> RGM [(Uci, (Double, Fen))]
doFetchCandidates action engineMoves = do
  let ucis     = action ^. edUcis
  let pPrune   = action ^. edProbP
  let isPruned = action ^. edIsPruned
  parent
    <- throwMaybe ("Parent doesn't exist at ucis: " <> tshow ucis)
    <=< preuse
    $ moveTree . MT.traverseUcis ucis
  let pFen           = parent ^. nodeFen
  (rStats, lcM)      <- H.lichessMoves pFen
  (rMStats, maybeMM) <- H.maybeMastersMoves pFen
  pAgg               <- MT.fetchPAgg ucis
  color              <- view colorL
  stratSats          <- view $ strategy . satisficers
  stratOpt           <- view $ strategy . optimizer
  breadth            <- maxCandBreadth isPruned pAgg
  let candidates     = maybe lcM (adornProb lcM <$>) maybeMM
  let isMasters      = isJust maybeMM
  let bestMay        = parent ^? bestScoreL . _Just

  Stats.updateParentNominal pFen lichessStats rStats
  Stats.updateParentNominal pFen mastersStats rMStats

  initCands
    <- traverse (fetchFen ucis)
    <=< maybe (filterCandidates pFen pAgg candidates) pure
    <=< findUci candidates pFen <=< preview $ overridesL . ix pFen

  let candNodes
        =   take breadth
        .   Strat.strategicFilter stratSats bestMay
        .   sortBy (Strat.strategicCompare stratOpt color)
        $   Ngn.injectEngine engineMoves
        .   applyWhen isMasters (injectLichess lcM)
        .   initPosInfo isMasters
        <$> initCands

  cands' <- if null candNodes
    then firstEngine pPrune ucis engineMoves
    else pure candNodes
  traverse_ State.insertChildPosInfo cands'
  pure $ cands' ^.. folded . to (\(u, (f, p, _)) -> (u, (p, f)))

adornProb :: [(Uci, (Double, NodeStats))] -> (Uci, NodeStats) -> (Uci, (Double, NodeStats))
adornProb lcm (uci, ns) = (uci, (maybe 0 (view _1) (lookup uci lcm), ns))

fetchFen :: Vector Uci -> (Uci, (Double, NodeStats)) -> RGM (Uci, (Fen, Double, NodeStats))
fetchFen ucis (uci, (prob, ns)) = do
  pModule <- use chessHelpers
  fen <- liftIO . PyC.ucisToFen pModule $ snoc ucis uci
  pure (uci, (fen, prob, ns))

fromProcessed :: Vector Uci -> (Uci, (Double, Fen)) -> (Uci, TreeNode)
fromProcessed ucis (uci, (prob, fen))
  = ( uci
    , def & uciPath   .~ snoc ucis uci
          & nodeFen   .~ fen
          & probLocal .~ prob
    )

toAction :: EnumData -> Vector Uci -> [RGAction]
toAction (EnumData _ probP probA depth _) ucis
  = [ RGAEnumResps $ EnumData ucis probP probA (succ depth) False
    , RGACalcStats ucis
    ]

-- NOTE: can re-enable if the engine depth becomes more significant
firstEngine
  :: Double
  -> Vector Uci
  -> [EngineCandidate]
  -> RGM [(Uci, (Fen, Double, PosInfo))]
firstEngine _ ucis (_:_) = do
-- firstEngine pPrune ucis (ngn:_) = do
  -- pAgg <- MT.fetchPAgg ucis
  logWarnN $ "There are no candidates for ucis: " <> tshow ucis
  pure []
  -- logWarnN "Reverting to the top engine move"
  -- let uci = ngn ^. ngnUci
  -- pModule <- use chessHelpers
  -- fen <- liftIO . PyC.ucisToFen pModule $ snoc ucis uci
  -- pure
  --   [( uci
  --    , ( fen
  --      , def & posStats .~ mkRGStats pPrune pAgg
  --      )
  --    )]
firstEngine _ ucis [] = do
  logWarnN $ "There are no candidates for ucis: " <> tshow ucis
  -- logWarnN "Nor are there any engine moves."
  pure []

injectLichess :: [(Uci, (Double, NodeStats))] -> (Uci, (Fen, Double, PosInfo)) -> (Uci, (Fen, Double, PosInfo))
injectLichess lcM stats@(uci, _)
  = stats
  & _2
  . _3
  . posStats
  . lichessStats
  .~ (view _2 <$> lookup uci lcM)

initPosInfo
  :: Bool
  -> (Uci, (Fen, Double, NodeStats))
  -> (Uci, (Fen, Double, PosInfo))
initPosInfo isMasters (uci, (fen, prob, ns))
  = ( uci
    , ( fen
      , prob
      , def & posStats .~ stats
      )
    )
  where
    nsL = if isMasters then mastersStats else lichessStats
    stats = def & nsL ?~ ns

findUci
  :: [(Uci, (Double, NodeStats))]
  -> Fen
  -> Maybe Uci
  -> RGM (Maybe [(Uci, (Double, NodeStats))])
findUci _ _ Nothing = pure Nothing
findUci cands fen (Just u)
  = maybe logMiss (pure . Just . (:[]))
  $ cands ^? traversed . filtered (matchesUci u)
  where
    logMiss
      = logWarnN ("No data for override: " <> u <> " at FEN: " <> (fen ^. fenL))
      -- NOTE: could rewrite to store the Uci with no NodeStats
      -- , but skipping for now
      >> pure Nothing
    matchesUci u' (x, _) = x == u'

minFallbackWindow :: Double
minFallbackWindow = 0.9

-- | Filter available candidates for selection
filterCandidates
  :: Fen
  -> Double
  -> [(Uci, (Double, NodeStats))]
  -> RGM [(Uci, (Double, NodeStats))]
filterCandidates pFen pAgg mvs = do
  iMPl <- fromIntegral <$> view initMinPlays
  aMPl <- fromIntegral <$> view asymMinPlays
  -- Variable minPlays, but if too large, then the most played move used as ref
  let mpl
        = min (round (minFallbackWindow * fromIntegral maxPlays))
        . round
        $ aMPl + (iMPl - aMPl) * sqrt pAgg
  exMay <- preview $ exclusionsL . ix pFen
  pure . filter (g exMay) $ filter (f mpl) mvs
  where
    maxPlays = fromMaybe 0 . maximumMay $ mvs ^.. folded . _2 . _2 . playCount
    f mpl (_, (_, s)) = mpl < s ^. playCount
    g (Just exs) (u, _) = u `notElem` exs
    g Nothing _ = True

maxCandBreadth :: Bool -> Double -> RGM Int
maxCandBreadth _isPruned pAgg = do
  initBreadth <- view initCandBreadth
  asymBreadth <- view asymCandBreadth
  -- pure
  --   . round
  --   $ fromIntegral (initBreadth - asymBreadth) * pAgg
  --   + fromIntegral asymBreadth
  pure
    . round
    $ exp (log (initBreadth /. asymBreadth) * pAgg)
    * fromIntegral asymBreadth
  -- only go wider for candidates to be pruned
  -- if isPruned
  --   then
  --     pure
  --       . round
  --       $ fromIntegral (initBreadth - asymBreadth) * pAgg
  --       + fromIntegral asymBreadth
  --     -- pure
  --     --   . round
  --     --   $ exp (log (initBreadth /. asymBreadth) * pAgg)
  --     --   * fromIntegral asymBreadth
  --   else
  --     pure asymBreadth
