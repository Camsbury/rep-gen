{-# LANGUAGE TemplateHaskell #-}
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
  = Fen
  { _fenL :: Text
  } deriving (Show, Eq, Ord)
makeLenses ''Fen

instance Default Fen where
  def = Fen "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
