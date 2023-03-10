{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
--------------------------------------------------------------------------------
module RepGen.Strategy.Type where
--------------------------------------------------------------------------------
import Data.Aeson
  ( FromJSON(..)
  , (.:?)
  )
--------------------------------------------------------------------------------
import qualified Data.Aeson as J
import qualified Data.Aeson.Types as J
--------------------------------------------------------------------------------

data RGOptimizer
  = MinLoss
  | MaxWinOverLoss
  deriving (Show, Eq)
makePrisms ''RGOptimizer

instance Default RGOptimizer where
  def = MaxWinOverLoss

instance FromJSON RGOptimizer where
  parseJSON = J.withText "RGOptimizer" $ \t -> case toLower t of
    "minloss"        -> pure MinLoss
    "maxwinoverloss" -> pure MaxWinOverLoss
    _                -> J.parseFail "Invalid strategy"

data EngineFilter = EngineFilter
  { _engineAllowableLoss :: Double
  , _engineMoveCount     :: Int
  , _engineDepth         :: Int
  , _engineP             :: Bool
  } deriving (Show, Eq)
makeLenses ''EngineFilter

instance Default EngineFilter where
  def = EngineFilter
      { _engineAllowableLoss = 0.85
      -- | Not used much beyond as a default
      , _engineMoveCount     = 10
      , _engineDepth         = 10
      , _engineP             = True
      }

instance FromJSON EngineFilter where
  parseJSON
    = J.withObject "EngineFilter" $ \o -> do
      let updateIfPresent fName f c = maybe c (\a -> c & f .~ a) <$> o .:? fName
      updateIfPresent "engineAllowableLoss" engineAllowableLoss def
        >>= updateIfPresent "engineMoveCount" engineMoveCount
        >>= updateIfPresent "engineDepth" engineDepth
        >>= updateIfPresent "engineP" engineP

data RGSatisficers = RGSatisficers
  { _engineFilter :: EngineFilter
  } deriving (Show, Eq)
makeLenses ''RGSatisficers

instance Default RGSatisficers where
  def = RGSatisficers def

instance FromJSON RGSatisficers where
  parseJSON
    = J.withObject "RGSatisficers" $ \o -> do
      let updateIfPresent fName f c = maybe c (\a -> c & f .~ a) <$> o .:? fName
      updateIfPresent "engineFilter" engineFilter def

-- | The strategy for the repertoire generator.
-- There is one optimizer and any number of satisficers used.
data RGStrategy = RGStrategy
  { _optimizer   :: RGOptimizer
  , _satisficers :: RGSatisficers
  } deriving (Show, Eq)
makeLenses ''RGStrategy

instance Default RGStrategy where
  def = RGStrategy def def

instance FromJSON RGStrategy where
  parseJSON
    = J.withObject "RGStrategy" $ \o -> do
      let updateIfPresent fName f c = maybe c (\a -> c & f .~ a) <$> o .:? fName
      updateIfPresent "optimizer" optimizer def
        >>= updateIfPresent "satisficers" satisficers
