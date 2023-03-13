{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
--------------------------------------------------------------------------------
module RepGen.State.Type where
--------------------------------------------------------------------------------
import Foreign.Ptr
import RepGen.Type
import RepGen.Action.Type
import RepGen.PyChess.Type
import RepGen.MoveTree.Type
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
import qualified Text.Regex as RX
--------------------------------------------------------------------------------

data PosInfo
  = PosInfo
  { _posStats     :: RGStats
  , _chosenUci    :: Maybe Uci
  } deriving (Show, Eq)
makeLenses ''PosInfo

instance Default PosInfo where
  def = PosInfo def Nothing

instance FromJSON PosInfo where
  parseJSON
    = withObject "PosInfo"
    $ \o -> PosInfo
       <$> (o .: "posStats")
       <*> (o .: "chosenUci")

instance ToJSON PosInfo where
  toJSON posInfo =
    object
      [ "posStats"  J..= view posStats posInfo
      , "chosenUci" J..= view chosenUci posInfo
      ]

-- | FIXME: create HomogenizedFen type to ensure FENs remain valid

-- | Mapping to quickly find stats and chosen moves for a position
newtype PosToInfo
  = PosToInfo
  { _getPosToInfo :: Map Fen PosInfo
  } deriving (Show, Eq)
makeLenses ''PosToInfo

instance ToJSON PosToInfo where
  toJSON = toJSON . view getPosToInfo

instance FromJSON PosToInfo where
  parseJSON = fmap PosToInfo . parseJSON

-- | Remove portions of fen that have nothing to do with what is actually
-- playable from a position
homogenizeFen :: Fen -> Fen
homogenizeFen (Fen fen)
  = Fen . pack $ RX.subRegex (RX.mkRegex " [0-9]+ [0-9]+$") (unpack fen) ""

-- | Index into a PosToInfo with a homogenized Fen
ixPTI :: Fen -> Traversal' PosToInfo PosInfo
ixPTI fen = getPosToInfo . ix (homogenizeFen fen)

data RGState
  = RGState
  { _cloudLimitReached :: Bool
  , _posToInfo         :: PosToInfo
  , _chessHelpers      :: Ptr PyObject
  , _moveTree          :: TreeNode
  , _actionStack       :: [RGAction]
  } deriving (Show, Eq)
makeLenses ''RGState
