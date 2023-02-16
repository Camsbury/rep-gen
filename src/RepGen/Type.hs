module RepGen.Type where

import Prelude
import Control.Lens (makeLenses)

--------------------------------------------------------------------------------
--- Types
--------------------------------------------------------------------------------

data Color
  = White
  | Black
  deriving (Show, Eq)

flipColor :: Color -> Color
flipColor White = Black
flipColor Black = White

type RGError = Text

type San = Text
type Uci = Text
