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
        $ candidates ^.. folded . _2 . to (\f -> pTI ^? ixPTI f . posStats . rgScore . _Just . nom) . _Just
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

fetchCandidates :: EnumData -> RGM [(Uci, Fen)]
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
       pure $ children ^.. folded . to (\(u, n) -> (u, n ^. nodeFen))
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

doFetchCandidates :: EnumData -> [EngineCandidate] -> RGM [(Uci, Fen)]
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
  let candidates     = fromMaybe lcM maybeMM
  let isMasters      = isJust maybeMM
  let bestMay        = parent ^? bestScoreL . _Just

  Stats.updateParentNominal pFen lichessStats rStats
  Stats.updateParentNominal pFen mastersStats rMStats

  initCands
    <- traverse (fetchFen ucis)
    <=< maybe (filterCandidates pFen candidates) pure
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
  pure $ cands' ^.. folded . to (\(u, (f, _)) -> (u, f))

fetchFen :: Vector Uci -> (Uci, NodeStats) -> RGM (Uci, (Fen, NodeStats))
fetchFen ucis (uci, ns) = do
  pModule <- use chessHelpers
  fen <- liftIO . PyC.ucisToFen pModule $ snoc ucis uci
  pure (uci, (fen, ns))

fromProcessed :: Vector Uci -> (Uci, Fen) -> (Uci, TreeNode)
fromProcessed ucis (uci, fen)
  = ( uci
    , def & uciPath .~ snoc ucis uci
          & nodeFen .~ fen
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
  -> RGM [(Uci, (Fen, PosInfo))]
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

injectLichess :: [(Uci, NodeStats)] -> (Uci, (Fen, PosInfo)) -> (Uci, (Fen, PosInfo))
injectLichess lcM stats@(uci, _)
  = stats
  & _2
  . _2
  . posStats
  . lichessStats
  .~ lookup uci lcM

initPosInfo
  :: Bool
  -> (Uci, (Fen, NodeStats))
  -> (Uci, (Fen, PosInfo))
initPosInfo isMasters (uci, (fen, ns))
  = ( uci
    , ( fen
      , def & posStats .~ stats
      )
    )
  where
    nsL = if isMasters then mastersStats else lichessStats
    stats = def & nsL ?~ ns

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
      = logWarnN ("No data for override: " <> u <> " at FEN: " <> (fen ^. fenL))
      -- NOTE: could rewrite to store the Uci with no NodeStats
      -- , but skipping for now
      >> pure Nothing
    matchesUci u' (x, _) = x == u'

-- | Filter available candidates for selection
filterCandidates
  :: Fen
  -> [(Uci, NodeStats)]
  -> RGM [(Uci, NodeStats)]
filterCandidates pFen mvs = do
  -- TODO: make this much smarter about what to filter
  -- don't want to be too restrictive, but also don't want to cut out all useful data
  -- there should be some notion of confidence ranges by sample size, and working with the lower end of that range
  -- can trim here based on how destructive to data a candidate is
  mpl <- view minPlays
  exMay <- preview $ exclusionsL . ix pFen
  pure . filter (g exMay) $ filter (f mpl) mvs
  where
    f mpl (_, s) = mpl < s ^. playCount
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
