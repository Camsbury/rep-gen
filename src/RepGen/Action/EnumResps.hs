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
  -> Double
  -> Double
  -> [(Uci, NodeStats)]
  -> [(Uci, NodeStats)]
  -> [(Uci, TreeNode)]
mergeMoves ucis pAgg pPrune lichessM mastersM
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
          , _responses = empty
          }
        )

wrapLCStats :: Vector Uci -> Double -> Double -> (Uci, NodeStats) -> (Uci, TreeNode)
wrapLCStats ucis pAgg pPrune (uci, lcm)
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
      , _responses = empty
      }
    )

processMoves :: EnumData -> Double -> RGM [(Uci, TreeNode)]
processMoves action pAgg = do
  let ucis = action ^. edUcis
  maybeMastersM <- maybeMastersMoves ucis
  lichessM <- filterMoves action pAgg =<< lichessMoves ucis
  let pPrune = action ^. edProb
  pure $ maybe
           (wrapLCStats ucis pAgg pPrune <$> lichessM)
           (mergeMoves ucis pAgg pPrune lichessM)
           maybeMastersM

-- | Turn viable responses into actions
toAction :: EnumData -> TreeNode -> [RGAction]
toAction action node
  = [ RGAEnumCands
      $ EnumData
      { _edUcis = node ^. uciPath
      , _edProb = node ^. rgStats . probPrune
      , _edDepth = action ^. edDepth
      , _edIsPruned = action ^. edIsPruned
      }
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

runAction :: EnumData -> RGM ()
runAction action = do
  let ucis = action ^. edUcis
  pAgg <- throwMaybe ("Node doesn't exist at: " <> intercalate "," ucis)
       <=< preuse $ moveTree . traverseUcis ucis . rgStats . probAgg
  processed <- processMoves action pAgg
  moveTree . traverseUcis ucis . responses .= fromList processed
  toActOn <- filterMinRespProb (action ^. edProb) pAgg processed
  actionStack %= ((toAction action =<< reverse toActOn) ++)
