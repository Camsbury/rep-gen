{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
--------------------------------------------------------------------------------
module RepGen.Stats.Type where
--------------------------------------------------------------------------------
import RepGen.Type
--------------------------------------------------------------------------------
import Data.Aeson
  ( FromJSON(..)
  , ToJSON(..)
  , object
  , withObject
  , (.:)
  , (.:?)
  )
--------------------------------------------------------------------------------
import qualified Data.Aeson as J
--------------------------------------------------------------------------------

-- | Provide a nominal and aggregate statistic
data RGStat
  = RGStat
  { _nom :: Double
  , _agg :: Double
  } deriving (Show, Eq)
makeLenses ''RGStat

-- | Convenience function for a initializing a stat
mkRGStat :: Double -> RGStat
mkRGStat d = RGStat d d

-- | Node stats per source (lichess, masters)
data NodeStats
  = NodeStats
  { _whiteWins :: RGStat
  , _blackWins :: RGStat
  , _playCount :: Int
  } deriving (Show, Eq)
makeLenses ''NodeStats

myWins :: Color -> Lens' NodeStats RGStat
myWins White = whiteWins
myWins Black = blackWins

oppWins :: Color -> Lens' NodeStats RGStat
oppWins White = blackWins
oppWins Black = whiteWins

data RGStats
  = RGStats
  { _lichessStats :: Maybe NodeStats
  , _mastersStats :: Maybe NodeStats
  , _rgScore      :: Maybe RGStat
  -- | This is the best score seen so far (used for filtering)
  } deriving (Show, Eq)
makeLenses ''RGStats

instance Default RGStats where
  def = RGStats Nothing Nothing Nothing

instance ToJSON RGStats where
  toJSON stats =
    object
      [ "lichessStats" J..= view lichessStats stats
      , "mastersStats" J..= view mastersStats stats
      , "rgScore"      J..= view rgScore      stats
      ]

instance ToJSON NodeStats where
  toJSON ns =
    object
      [ "whiteWins" J..= view whiteWins ns
      , "blackWins" J..= view blackWins ns
      , "playCount" J..= view playCount ns
      ]

instance ToJSON RGStat where
  toJSON stat =
    object
      [ "nom" J..= view nom stat
      , "agg" J..= view agg stat
      ]

instance FromJSON RGStats where
  parseJSON
    = withObject "RGStats"
    $ \o -> RGStats
      <$> (o .:? "lichessStats")
      <*> (o .:? "mastersStats")
      <*> (o .:? "rgScore")

instance FromJSON NodeStats where
  parseJSON
    = withObject "NodeStats"
    $ \o -> NodeStats
      <$> (o .: "whiteWins")
      <*> (o .: "blackWins")
      <*> (o .: "playCount")

instance FromJSON RGStat where
  parseJSON
    = withObject "RGStat"
    $ \o -> RGStat <$> (o .: "nom") <*> (o .: "agg")
