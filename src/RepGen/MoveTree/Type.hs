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
  )
--------------------------------------------------------------------------------
import qualified Data.Aeson as J
--------------------------------------------------------------------------------

-- | A node in the tree of candidates and responses in the generated repertoire
data TreeNode
  = TreeNode
  { _uciPath       :: Vector Uci
  , _nodeFen       :: Fen
  , _nodeResponses :: Vector (Uci, TreeNode)
  , _removed       :: Bool
  , _transposes    :: Bool
  } deriving (Show, Eq)
makeLenses ''TreeNode

instance Default TreeNode where
  def = TreeNode empty def empty False False

instance ToJSON TreeNode where
  toJSON node =
    object
      [ "uciPath"       J..= view uciPath       node
      , "nodeFen"       J..= view nodeFen       node
      , "nodeResponses" J..= view nodeResponses node
      , "removed"       J..= view removed       node
      , "transposes"    J..= view transposes    node
      ]

instance FromJSON TreeNode where
  parseJSON
    = withObject "TreeNode"
    $ \o -> TreeNode
      <$> (o .: "uciPath")
      <*> (o .: "nodeFen")
      <*> (o .: "nodeResponses")
      <*> (o .: "removed")
      <*> (o .: "transposes")
