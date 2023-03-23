--------------------------------------------------------------------------------
module RepGen.State
  ( initState
  , collectInfo
  , insertChildPosInfo
  ) where
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
import qualified Data.Aeson             as J
import qualified RepGen.Lichess.History as H
import qualified RepGen.Engine          as Ngn
import qualified RepGen.MoveTree        as MT
import qualified System.IO              as Sys

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
  rFrom <- view resumeFrom
  case rFrom of
    Nothing -> do
      actions <- initActions <$> view colorL
      iPTI <- initPosToInfo pModule
      logDebugN $ "Actions: " <> tshow actions
      logInfoN "Finished initializing state"
      iPA
        <- throwMaybe "No initial lichess stats"
        $ iPTI ^? ixPTI def . posStats . lichessStats . _Just . prob
      pure $ RGState
           { _cloudLimitReached = False
           , _posToInfo         = iPTI
           , _chessHelpers      = pModule
           , _moveTree
             = TreeNode
             { _uciPath       = empty
             , _nodeFen       = def
             , _nodeResponses = empty
             , _removed       = False
             , _transposes    = False
             , _bestScoreL    = iPTI ^? ixPTI def . posStats . rgScore . _Just . nom
             , _probPrune     = 1
             , _probAgg       = iPA
             }
           , _actionStack       = actions
           }
    Just (treePath, infoPath) -> do
      tree
        <- throwEither
        . first pack
        . J.eitherDecode
        . fromStrict
        . encodeUtf8
        . pack
        <=< liftIO
        . Sys.readFile
        $ unpack treePath

      info
        <- throwEither
        . first pack
        . J.eitherDecode
        . fromStrict
        . encodeUtf8
        . pack
        <=< liftIO
        . Sys.readFile
        $ unpack infoPath

      logInfoN "Parsed resumable data"

      (`execStateT`
       (RGState
        { _cloudLimitReached = False
        , _posToInfo         = info
        , _chessHelpers      = pModule
        , _moveTree          = tree
        , _actionStack       = []
        })) $ do
        actions <- resumeActions tree info =<< view colorL

        actionStack .= actions

        logInfoN "Built actions for resuming"


initActions :: Color -> [RGAction]
initActions White
  = [ RGAEnumCands
      $ EnumData
      { _edUcis     = []
      , _edProbP    = 1
      , _edProbA    = 1
      , _edDepth    = 1
      , _edIsPruned = True
      }
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
      stats' = stats & rgScore    .~ scoreStat
  pure . PosToInfo $ mapFromList [(homogenizeFen def, def & posStats .~ stats')]

collectInfo :: (Uci, TreeNode) -> RGM (Uci, (Fen, PosInfo))
collectInfo (uci, node) = do
  let fen = node ^. nodeFen
  mInfo <- preuse $ posToInfo . ixPTI fen
  info <- throwMaybe ("No position info for fen: " <> tshow fen) mInfo
  pure (uci, (fen, info))

insertChildPosInfo :: (Uci, (Fen, PosInfo)) -> RGM ()
insertChildPosInfo (_, (fen, pInfo)) = do
  mPInfo <- preuse $ posToInfo . ixPTI fen
  when (isNothing mPInfo) $
    posToInfo . getPosToInfo . at (homogenizeFen fen) ?= pInfo

resumeActions
  :: ( MonadReader RGConfig m
    , MonadError  RGError  m
    , MonadLogger m
    , MonadIO m
    , MonadState RGState m
    )
  => TreeNode
  -> PosToInfo
  -> Color
  -> m [RGAction]
resumeActions root pTI White
  = doResumeActions root pTI True
resumeActions root pTI Black
  = doResumeActions root pTI False

doResumeActions
  :: ( MonadReader RGConfig m
    , MonadError  RGError  m
    , MonadLogger m
    , MonadIO m
    , MonadState RGState m
    )
  => TreeNode
  -> PosToInfo
  -> Bool
  -> m [RGAction]
doResumeActions node pTI True = do
  let ucis = node ^. uciPath
  child
   <- throwMaybe ("Valid child doesn't exist for ucis: " <> tshow ucis)
   $ node ^? MT.validChildrenT . _2

  -- apply next step to the responses of the child
  (++ [RGATransStats ucis]) <$> doResumeActions child pTI False
doResumeActions node pTI False
  = fmap fold . traverse toActions $ node ^.. nodeResponses . folded . _2
  where
    toActions
      :: ( MonadReader RGConfig m
        , MonadError  RGError  m
        , MonadLogger m
        , MonadIO m
        , MonadState RGState m
        )
      => TreeNode
      -> m [RGAction]
    toActions child = do
      mpa <- view minProbAgg
      let ucis = child ^. uciPath
      let pAgg = child ^. probAgg
      if child ^. removed
        then
          if pAgg > mpa
            then do
              moveTree . MT.traverseUcis ucis . removed .= False
              pure
                [ RGAEnumCands
                  $ EnumData
                  { _edUcis     = ucis
                  , _edProbP    = 1
                  , _edProbA    = pAgg
                  , _edDepth    = 1
                  , _edIsPruned = True
                  }
                , RGAPruneCands ucis
                , RGATransStats ucis
                ]
            else
              pure []
        else
          doResumeActions child pTI True


