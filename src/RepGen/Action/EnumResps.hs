--------------------------------------------------------------------------------
module RepGen.Action.EnumResps
  ( module RepGen.Action.EnumResps
  ) where
--------------------------------------------------------------------------------

import RepGen.Action.Type
import RepGen.Config.Type
import RepGen.Lichess.History
import RepGen.Monad
import RepGen.MoveTree.Type
import RepGen.State.Type
import RepGen.Stats.Type
import RepGen.Type
--------------------------------------------------------------------------------
import qualified RepGen.PyChess as PyC
--------------------------------------------------------------------------------

filterMoves
  :: EnumData
  -> Double
  -> [(Uci, NodeStats)]
  -> RGM [(Uci, NodeStats)]
filterMoves action pAgg mvs = do
  mpa <- view minProbAgg
  mpl <- view minPlays
  pure . filter (g mpl) $ filter (f mpa) mvs
  where
    isPruned = action ^. edIsPruned
    f mpa (_, s) = not isPruned || mpa < (pAgg * (s ^. prob))
    g mpl (_, s) = mpl < s ^. playCount

mergeMoves
  :: Vector Uci
  -> Fen
  -> Double
  -> Double
  -> [(Uci, NodeStats)]
  -> [(Uci, NodeStats)]
  -> [(Uci, TreeNode)]
mergeMoves ucis fen pAgg pPrune lichessM mastersM
  = f <$> lichessM
  where
    findBy _ [] = Nothing
    findBy uci ((mUci, mStats):rest)
      | uci == mUci = Just mStats
      | otherwise = lookup uci rest
    f (uci, lcm)
      = ( uci
        , TreeNode
          { _rgStats
            = RGStats
            { _lichessStats = Just lcm
            , _mastersStats = findBy uci mastersM
            , _score        = Nothing
            , _probPrune    = pPrune * lcm ^. prob
            , _probAgg      = pAgg * lcm ^. prob
            }
          , _uciPath = snoc ucis uci
          , _nodeFen = fen
          , _responses = empty
          }
        )

wrapLCStats :: Vector Uci -> Fen -> Double -> Double -> (Uci, NodeStats) -> (Uci, TreeNode)
wrapLCStats ucis fen pAgg pPrune (uci, lcm)
  = ( uci
    , TreeNode
      { _rgStats
        = RGStats
        { _lichessStats = Just lcm
        , _mastersStats = Nothing
        , _score        = Nothing
        , _probPrune    = pPrune * lcm ^. prob
        , _probAgg      = pAgg * lcm ^. prob
        }
      , _uciPath = snoc ucis uci
      , _nodeFen = fen
      , _responses = empty
      }
    )

processMoves :: EnumData -> Double -> RGM [(Uci, TreeNode)]
processMoves action pAgg = do
  let ucis = action ^. edUcis
  fen <- liftIO $ PyC.ucisToFen ucis
  maybeMastersM <- maybeMastersMoves fen
  lichessM <- filterMoves action pAgg =<< lichessMoves fen
  let pPrune = action ^. edProbP
  pure $ maybe
           (wrapLCStats ucis fen pAgg pPrune <$> lichessM)
           (mergeMoves ucis fen pAgg pPrune lichessM)
           maybeMastersM

initProcessMoves :: Vector Uci -> Double -> RGM [(Uci, TreeNode)]
initProcessMoves ucis pAgg = do
  fen <- liftIO $ PyC.ucisToFen ucis
  maybeMastersM <- maybeMastersMoves fen
  lichessM <- lichessMoves fen
  pure $ maybe
           (wrapLCStats ucis fen pAgg 1 <$> lichessM)
           (mergeMoves ucis fen pAgg 1 lichessM)
           maybeMastersM

-- | Turn viable responses into actions
toAction :: EnumData -> TreeNode -> [RGAction]
toAction action node
  = [ RGAEnumCands
      $ EnumData
      { _edUcis = node ^. uciPath
      , _edProbP = node ^. rgStats . probPrune
      , _edDepth = action ^. edDepth
      , _edIsPruned = action ^. edIsPruned
      }
    , RGATransStats $ node ^. uciPath
    ]

-- | Turn viable responses into actions
initToAction ::  TreeNode -> [RGAction]
initToAction node
  = [ RGAEnumCands
      $ EnumData
      { _edUcis = node ^. uciPath
      , _edProbP = node ^. rgStats . probPrune
      , _edDepth = 0
      , _edIsPruned = False
      }
    , RGAPruneCands $ node ^. uciPath
    , RGATransStats $ node ^. uciPath
    ]

-- | Filter resopnses to act on by minimum response probability
filterMinRespProb
  :: Double
  -> Double
  -> [(Uci, TreeNode)]
  -> RGM [TreeNode]
filterMinRespProb pPrune pAgg resps = do
  irp <- view initRespProb
  arp <- view asymRespProb
  let minProb = exp (negate pAgg) * succ (arp + irp) + irp - 1
  let f (_, rNode) =
        maybe
          False
          (\x -> pPrune * x > minProb)
          (rNode ^? rgStats . lichessStats . _Just . prob)
  pure . fmap (view _2) . filter f $ resps

fetchPAgg :: Vector Uci -> RGM Double
fetchPAgg ucis
  = throwMaybe ("Node doesn't exist at: " <> intercalate "," ucis)
  <=< preuse $ moveTree . traverseUcis ucis . rgStats . probAgg

runAction :: EnumData -> RGM ()
runAction action = do
  let ucis = action ^. edUcis
  pAgg <- fetchPAgg ucis
  processed <- processMoves action pAgg
  moveTree . traverseUcis ucis . responses .= fromList processed
  toActOn <- filterMinRespProb (action ^. edProbP) pAgg processed
  actionStack %= ((toAction action =<< reverse toActOn) ++)

initRunAction :: Vector Uci -> RGM ()
initRunAction ucis = do
  pAgg <- fetchPAgg ucis
  processed <- initProcessMoves ucis pAgg
  moveTree . traverseUcis ucis . responses .= fromList processed
  toActOn <- filterMinRespProb 1 pAgg processed
  actionStack %= ((initToAction =<< reverse toActOn) ++)

