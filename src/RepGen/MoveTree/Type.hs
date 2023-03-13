{-# LANGUAGE TemplateHaskell #-}
--------------------------------------------------------------------------------
module RepGen.MoveTree.Type where
--------------------------------------------------------------------------------
import RepGen.Type
--------------------------------------------------------------------------------
import Data.Aeson
  ( FromJSON(..)
  , ToJSON(..)
  , object
  , withObject
  , (.:)
  , (.:?)
  )
--------------------------------------------------------------------------------
import qualified Data.Aeson as J
--------------------------------------------------------------------------------

type ProbPrune = Double
type ProbAgg   = Double

-- | A node in the tree of candidates and responses in the generated repertoire
data TreeNode
  = TreeNode
  { _uciPath       :: Vector Uci
  , _nodeFen       :: Fen
  , _nodeResponses :: Vector (Uci, TreeNode)
  , _removed       :: Bool
  , _transposes    :: Bool
  , _bestScoreL    :: Maybe Double
  , _probPrune     :: ProbPrune
  , _probAgg       :: ProbAgg
  } deriving (Show, Eq)
makeLenses ''TreeNode

instance Default TreeNode where
  def = TreeNode empty def empty False False Nothing 1 1

instance ToJSON TreeNode where
  toJSON node =
    object
      [ "uciPath"       J..= view uciPath       node
      , "nodeFen"       J..= view nodeFen       node
      , "nodeResponses" J..= view nodeResponses node
      , "removed"       J..= view removed       node
      , "transposes"    J..= view transposes    node
      , "bestScoreL"    J..= view bestScoreL    node
      , "probPrune"     J..= view probPrune     node
      , "probAgg"       J..= view probAgg       node
      ]

instance FromJSON TreeNode where
  parseJSON
    = withObject "TreeNode"
    $ \o -> TreeNode
      <$> (o .:  "uciPath")
      <*> (o .:  "nodeFen")
      <*> (o .:  "nodeResponses")
      <*> (o .:  "removed")
      <*> (o .:  "transposes")
      <*> (o .:? "bestScoreL")
      <*> (o .:  "probPrune")
      <*> (o .:  "probAgg")
