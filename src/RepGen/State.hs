--------------------------------------------------------------------------------
module RepGen.State where
--------------------------------------------------------------------------------
import RepGen.Type
import RepGen.Action.Type
import RepGen.Config.Type
import RepGen.MoveTree.Type
import RepGen.Score.Type
import RepGen.State.Type
import RepGen.Stats.Type
--------------------------------------------------------------------------------
import qualified RepGen.Lichess.History as H
import qualified RepGen.Engine as Ngn
--------------------------------------------------------------------------------

-- | Initialize the state of the repertoire generator
initState
  :: ( MonadReader RGConfig m
    , MonadError  RGError  m
    , MonadLogger m
    , MonadIO m
    )
  => m RGState
initState = do
  logInfoN "Initializing state"
  node <- baseNode
  actions <- initActions <$> view colorL
  -- logDebugN $ "Actions: " <> tshow actions
  logInfoN "Finished initializing state"
  pure $ def
       & moveTree .~ node
       & actionStack .~ actions

initActions :: Color -> [RGAction]
initActions White
  = [ RGAEnumCands $ EnumData [] 1 1 False
    , RGAPruneCands []
    , RGATransStats []
    ]
initActions Black
  = [ RGAInitResps []
    , RGACalcStats []
    ]

baseNode
  :: ( MonadReader RGConfig m
    , MonadError  RGError  m
    , MonadLogger m
    , MonadIO m
    )
  => m TreeNode
baseNode = do
  stats <- H.initialStats
  score <- Ngn.fenToScore def
  let scoreStat = mkRGStat . view scoreL <$> score
      stats' = stats & rgScore .~ scoreStat
  pure $ TreeNode stats' empty def empty False
