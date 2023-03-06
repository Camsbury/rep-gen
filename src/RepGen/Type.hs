{-# LANGUAGE TemplateHaskell #-}
--------------------------------------------------------------------------------
module RepGen.Type where
--------------------------------------------------------------------------------
import Data.Aeson
  ( FromJSON(..)
  , ToJSON(..)
  , ToJSONKey(..)
  , withText
  )
import Data.Aeson.Types
  ( toJSONKeyText
  )
--------------------------------------------------------------------------------
import qualified Data.Aeson as J
--------------------------------------------------------------------------------

data Color
  = White
  | Black
  deriving (Show, Eq)

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
