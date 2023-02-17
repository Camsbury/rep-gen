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
