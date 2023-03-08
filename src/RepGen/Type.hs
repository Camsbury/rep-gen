{-# LANGUAGE TemplateHaskell #-}
--------------------------------------------------------------------------------
module RepGen.Type where
--------------------------------------------------------------------------------
import Data.Aeson
  ( FromJSON(..)
  , FromJSONKey(..)
  , FromJSONKeyFunction(..)
  , ToJSON(..)
  , ToJSONKey(..)
  , withText
  )
import Data.Aeson.Types
  ( toJSONKeyText
  , parseFail
  )
--------------------------------------------------------------------------------
import qualified Data.Aeson as J
--------------------------------------------------------------------------------

data Color
  = White
  | Black
  deriving (Show, Eq)

instance FromJSON Color where
  parseJSON = withText "Color" $ \t -> case toLower t of
    "white" -> pure      White
    "black" -> pure      Black
    _       -> parseFail "Invalid color"

type RGError = Text

type San = Text
type Uci = Text

newtype Fen
  = Fen
  { _fenL :: Text
  } deriving (Show, Eq, Ord)
makeLenses ''Fen

instance Default Fen where
  def = Fen "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

instance ToJSON Fen where
  toJSON (Fen fen) = J.String fen

instance ToJSONKey Fen where
  toJSONKey = toJSONKeyText $ view fenL

instance FromJSON Fen where
  parseJSON = withText "Fen" $ \t -> pure $ Fen t

instance FromJSONKey Fen where
  fromJSONKey = FromJSONKeyText Fen
