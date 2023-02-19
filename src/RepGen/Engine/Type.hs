{-# LANGUAGE TemplateHaskell #-}
--------------------------------------------------------------------------------
module RepGen.Engine.Type
  ( module RepGen.Engine.Type
  ) where
--------------------------------------------------------------------------------
import RepGen.Type
import RepGen.Score.Type
--------------------------------------------------------------------------------

data EngineCandidate
  = EngineCandidate
  { _ngnUci :: !Uci
  , _ngnScore  :: Score
  } deriving (Show, Eq)
makeLenses ''EngineCandidate
