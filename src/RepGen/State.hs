--------------------------------------------------------------------------------
module RepGen.State where
--------------------------------------------------------------------------------
import Foreign.Ptr
import RepGen.Monad
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
  actions <- initActions <$> view colorL
  iPTI <- initPosToInfo pModule
  logDebugN $ "Actions: " <> tshow actions
  logInfoN "Finished initializing state"
  pure $ RGState
       { _cloudLimitReached = False
       , _posToInfo         = iPTI
       , _chessHelpers      = pModule
       , _moveTree          = TreeNode empty def empty False False
       , _actionStack       = actions
       }

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

initPosToInfo
  :: ( MonadReader RGConfig m
    , MonadError  RGError  m
    , MonadLogger m
    , MonadIO m
    )
  => Ptr PyObject
  -> m PosToInfo
initPosToInfo pModule = do
  stats <- H.initialStats
  score <- Ngn.fenToScore pModule def
  let scoreStat = mkRGStat . view scoreL <$> score
      stats' = stats & rgScore   .~ scoreStat
                     & bestScore .~ score ^? _Just . scoreL
  pure $ mapFromList [(def, def & posStats .~ stats')]

collectInfo :: (Uci, TreeNode) -> RGM (Uci, (Fen, PosInfo))
collectInfo (uci, node) = do
  let fen = node ^. nodeFen
  mInfo <- preuse $ posToInfo . ix fen
  info <- throwMaybe ("No position info for fen: " <> tshow fen) mInfo
  pure (uci, (fen, info))

insertChildPosInfo :: (Uci, (Fen, PosInfo)) -> RGM ()
insertChildPosInfo (_, (fen, pInfo))
  = posToInfo . at fen ?= pInfo
