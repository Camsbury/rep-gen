--------------------------------------------------------------------------------
module RepGen.State where
--------------------------------------------------------------------------------
import Foreign.Ptr
import RepGen.Type
import RepGen.Action.Type
import RepGen.Config.Type
import RepGen.MoveTree.Type
import RepGen.PyChess.Type
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
  => Ptr PyObject
  -> m RGState
initState pModule = do
  logInfoN "Initializing state"
  node <- baseNode pModule
  actions <- initActions <$> view colorL
  logDebugN $ "Actions: " <> tshow actions
  logInfoN "Finished initializing state"
  pure $ RGState False pModule node actions

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
  => Ptr PyObject
  -> m TreeNode
baseNode pModule = do
  stats <- H.initialStats
  score <- Ngn.fenToScore pModule def
  let scoreStat = mkRGStat . view scoreL <$> score
      stats' = stats & rgScore .~ scoreStat
  pure $ TreeNode stats' empty def empty False
