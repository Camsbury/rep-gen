--------------------------------------------------------------------------------
module RepGen.State where
--------------------------------------------------------------------------------
import RepGen.Type
import RepGen.Action.Type
import RepGen.Config.Type
import RepGen.MoveTree
import RepGen.MoveTree.Type
import RepGen.Score.Type
import RepGen.State.Type
import RepGen.Stats.Type
--------------------------------------------------------------------------------
import qualified RepGen.PyChess as PyC
import qualified RepGen.Lichess.History as H
import qualified RepGen.Engine as Ngn
--------------------------------------------------------------------------------

-- | Initialize the state of the repertoire generator
initState
  :: ( MonadReader RGConfig m
    , MonadError  RGError  m
    , MonadIO m
    )
  => m RGState
initState = do
  color <- view colorL
  node <- initTree
  pure . RGState node $ initActions color node

initTree
  :: ( MonadReader RGConfig m
    , MonadError  RGError  m
    , MonadIO m
    )
  => m TreeNode
initTree = do
  moves <- view startingMoves
  ucis <- liftIO $ PyC.sansToUcis moves
  foldl' addChild (baseNode empty 1) ucis

initActions :: Color -> TreeNode -> [RGAction]
initActions color node
  = if isMyTurn color
    then
    [ RGAEnumCands $ EnumData ucis 1 1 False
    , RGAPruneCands ucis
    , RGATransStats ucis
    ]
    else
    [ RGAInitResps ucis
    , RGACalcStats ucis
    ]
  where
    leaf = leafNode node
    ucis = leaf ^. uciPath
    isMyTurn White = even $ length ucis
    isMyTurn Black = odd  $ length ucis

baseNode
  :: ( MonadReader RGConfig m
    , MonadError  RGError  m
    , MonadIO m
    )
  => Vector Uci
  -> Double
  -> m TreeNode
baseNode ucis pAgg = do
  fen <- liftIO $ PyC.ucisToFen ucis
  stats <- H.initialStats fen pAgg
  score <- Ngn.fenToScore fen
  let scoreStat = mkRGStat . view scoreL <$> score
      stats' = stats & rgScore .~ scoreStat
  pure $ TreeNode stats' ucis fen empty False

addChild
  :: ( MonadReader RGConfig m
    , MonadError  RGError  m
    , MonadIO m
    )
  => m TreeNode
  -> Uci
  -> m TreeNode
addChild mNode uci = do
  node <- mNode
  let leaf = leafNode node
  let ucis = snoc (leaf ^. uciPath) uci
  newLeaf <- baseNode ucis $ node ^. rgStats . probAgg
  pure $ node & traverseUcis ucis .~ newLeaf

leafNode :: TreeNode -> TreeNode
leafNode node = f $ node ^? responses . folded . _2
  where
    f Nothing      = node
    f (Just child) = leafNode child
