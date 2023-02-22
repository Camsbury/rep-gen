{-# LANGUAGE TemplateHaskell #-}
--------------------------------------------------------------------------------
module RepGen.MoveTree.Type where
--------------------------------------------------------------------------------
import RepGen.Type
import RepGen.Stats.Type
--------------------------------------------------------------------------------
import Data.Aeson
  ( FromJSON(..)
  , ToJSON(..)
  , object
  , withObject
  , (.:)
  )
--------------------------------------------------------------------------------
import qualified Data.Aeson as J
--------------------------------------------------------------------------------

-- | A node in the tree of candidates and responses in the generated repertoire
data TreeNode
  = TreeNode
  { _rgStats   :: RGStats
  , _uciPath   :: Vector Uci
  , _nodeFen   :: Fen
  , _responses :: Vector (Uci, TreeNode)
  , _removed   :: Bool
  } deriving (Show, Eq)
makeLenses ''TreeNode

instance Default TreeNode where
  def = TreeNode def empty def empty False

instance ToJSON TreeNode where
  toJSON node =
    object
      [ "rgStats"   J..= view rgStats   node
      , "uciPath"   J..= view uciPath   node
      , "nodeFen"   J..= view nodeFen   node
      , "responses" J..= view responses node
      , "removed"   J..= view removed   node
      ]

instance FromJSON TreeNode where
  parseJSON
    = withObject "TreeNode"
    $ \o -> TreeNode
      <$> (o .: "rgStats")
      <*> (o .: "uciPath")
      <*> (o .: "nodeFen")
      <*> (o .: "responses")
      <*> (o .: "removed")
