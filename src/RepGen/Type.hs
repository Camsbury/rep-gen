module RepGen.Type where

--------------------------------------------------------------------------------
--- Types
--------------------------------------------------------------------------------

data Color
  = White
  | Black
  deriving (Show, Eq)

type RGError = Text

type San = Text
type Uci = Text

newtype Fen
  = Fen Text
  deriving (Show, Eq)

instance Default Fen where
  def = Fen "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
