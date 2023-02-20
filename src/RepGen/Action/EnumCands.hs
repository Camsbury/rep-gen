--------------------------------------------------------------------------------
module RepGen.Action.EnumCands
  ( runAction
  ) where
--------------------------------------------------------------------------------

import RepGen.Action.Type
import RepGen.Config.Type
import RepGen.Engine.Type
import RepGen.Monad
import RepGen.Stats.Type
import RepGen.Type
--------------------------------------------------------------------------------
import RepGen.Lichess.History ( maybeMastersMoves, lichessMoves )
--------------------------------------------------------------------------------
import qualified RepGen.PyChess  as PyC
import qualified RepGen.Engine   as Ngn
import qualified RepGen.MoveTree as MT
--------------------------------------------------------------------------------

runAction :: EnumData -> RGM ()
runAction action = do
  let ucis = action ^. edUcis
  let depth = succ $ action ^. edDepth
  pAgg <- MT.fetchPAgg ucis
  fen <- liftIO $ PyC.ucisToFen ucis
  eDepth <- view $ engineConfig . engineDepth
  eMC <- view $ engineConfig . engineMoveCount
  lcM <- lichessMoves fen
  maybeMM <- maybeMastersMoves fen
  let candidates = fromMaybe lcM maybeMM
  engineMoves <- Ngn.fenToEngineCandidates fen eDepth eMC
  maybeOverride <- preview $ overridesL . ix fen
  maybeMatch <- findUci candidates fen maybeOverride
  filteredCands
    <- maybe (filterCandidates candidates engineMoves) pure maybeMatch
  -- TODO: create the TreeNode with all the stuff you've got,
    -- if filteredCands is empty, TODO: log it loudly, and choose the first engine move (this may have empty stats)
    --look to EnumResps to find some good TreeNode creation
  -- TODO: stick it in the tree
  -- TODO: add actions to the stack
  undefined

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

