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
  logInfoN $ "Enumerating Candidates for: " <> tshow ucis
  candidates <- fetchCandidates action
  sDepth <- view searchDepth
  pTI <- use posToInfo
  let maybeBestScore
        = maximumMay
        $ candidates ^.. folded . _2 . to (\f -> pTI ^? ix f . posStats . rgScore . _Just . nom) . _Just
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
     Just _ ->
       pure $ children ^.. folded . to (\(u, n) -> (u, n ^. nodeFen))
     Nothing -> do
       candidates <- doFetchCandidates action
       let nodes = fromProcessed ucis <$> candidates
       moveTree . MT.traverseUcis ucis . nodeResponses .= fromList nodes
       pure candidates


doFetchCandidates :: EnumData -> RGM [(Uci, Fen)]
doFetchCandidates action = do
  let ucis   = action ^. edUcis
  let pPrune = action ^. edProbP
  parent
    <- throwMaybe ("Parent doesn't exist at ucis: " <> tshow ucis)
    <=< preuse
    $ moveTree . MT.traverseUcis ucis
  let pFen = parent ^. nodeFen
  (rStats, lcM)      <- H.lichessMoves pFen
  (rMStats, maybeMM) <- H.maybeMastersMoves pFen
  engineMoves        <- Ngn.fenToEngineCandidates pFen
  pAgg               <- MT.fetchPAgg ucis
  color              <- view colorL
  stratSats          <- view $ strategy . satisficers
  stratOpt           <- view $ strategy . optimizer
  breadth            <- maxCandBreadth pAgg
  let candidates     = fromMaybe lcM maybeMM
  let isMasters      = isJust maybeMM

  Stats.updateParentNominal pFen lichessStats rStats
  Stats.updateParentNominal pFen mastersStats rMStats

  initCands
    <- traverse (fetchFen ucis)
    <=< maybe (filterCandidates candidates) pure
    <=< findUci candidates pFen <=< preview $ overridesL . ix pFen

  let candNodes
        =   take breadth
        .   Strat.strategicFilter stratSats
        .   sortBy (Strat.strategicCompare stratOpt color)
        $   Ngn.injectEngine engineMoves
        .   applyWhen isMasters (injectLichess lcM)
        .   initPosInfo isMasters pAgg pPrune
        <$> initCands
  cands' <- if null candNodes
    then firstEngine pPrune pFen ucis engineMoves
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
    , TreeNode (snoc ucis uci) fen mempty False
    )

toAction :: EnumData -> Vector Uci -> [RGAction]
toAction (EnumData _ probP depth _) ucis
  = [ RGAEnumResps $ EnumData ucis probP (succ depth) False
    , RGACalcStats ucis
    ]

firstEngine
  :: Double
  -> Fen
  -> Vector Uci
  -> [EngineCandidate]
  -> RGM [(Uci, (Fen, PosInfo))]
firstEngine pPrune fen ucis (ngn:_) = do
  pAgg <- MT.fetchPAgg ucis
  logWarnN $ "There are no candidates for FEN: " <> fen ^. fenL
  logWarnN "Reverting to the top engine move"
  let uci = ngn ^. ngnUci
  pure
    [( uci
     , ( fen
       , def & posStats .~ mkRGStats pPrune pAgg
       )
     )]
firstEngine _ fen _ [] = do
  logWarnN $ "There are no candidates for FEN: " <> fen ^. fenL
  logWarnN "Nor are there any engine moves."
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
  -> Double
  -> Double
  -> (Uci, (Fen, NodeStats))
  -> (Uci, (Fen, PosInfo))
initPosInfo isMasters pAgg pPrune (uci, (fen, ns))
  = ( uci
    , ( fen
      , def & posStats .~ stats
      )
    )
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
      = logWarnN ("No data for override: " <> u <> " at FEN: " <> (fen ^. fenL))
      -- NOTE: could rewrite to store the Uci with no NodeStats
      -- , but skipping for now
      >> pure Nothing
    matchesUci u' (x, _) = x == u'

-- | Filter available candidates for selection
filterCandidates
  :: [(Uci, NodeStats)]
  -> RGM [(Uci, NodeStats)]
filterCandidates mvs = do
  mpl <- view minPlays
  pure $ filter (f mpl) mvs
  where
    f mpl (_, s) = mpl < s ^. playCount

maxCandBreadth :: Double -> RGM Int
maxCandBreadth pAgg = do
  initBreadth <- view initCandBreadth
  asymBreadth <- view asymCandBreadth
  pure . round $ exp (log (initBreadth /. asymBreadth) * pAgg) * fromIntegral asymBreadth
