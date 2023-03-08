{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
--------------------------------------------------------------------------------
module RepGen.Lichess.History.Type where
--------------------------------------------------------------------------------
import RepGen.Type
import Data.Time.Calendar (Year, MonthOfYear)
--------------------------------------------------------------------------------
import Data.Aeson
  ( FromJSON(..)
  , (.:)
  )
--------------------------------------------------------------------------------
import qualified Data.Aeson as J
import qualified Data.Aeson.Types as J
--------------------------------------------------------------------------------

data UniversalParams
  = UniversalParams
  { _moveCount :: Int -- moves
  , _fenParam  :: Fen
  } deriving (Eq, Show)
makeLenses ''UniversalParams

data GameSpeed
  = Bullet
  | Blitz
  | Rapid
  | Classical
  deriving (Eq, Show)

instance FromJSON GameSpeed where
  parseJSON
    = J.withText "GameSpeed"
    $ \t -> case toLower t of
      "bullet"    -> pure Bullet
      "blitz"     -> pure Blitz
      "rapid"     -> pure Rapid
      "classical" -> pure Classical
      _           -> J.parseFail "Invalid game speed"

speedText :: GameSpeed -> Text
speedText Bullet    = "bullet"
speedText Blitz     = "blitz"
speedText Rapid     = "rapid"
speedText Classical = "classical"

data LichessRating
  = L600
  | L1000
  | L1200
  | L1400
  | L1600
  | L1800
  | L2000
  | L2200
  | L2500
  deriving (Eq, Show)

instance FromJSON LichessRating where
  parseJSON
    = J.withScientific "LichessRating"
    $ \case
    600  -> pure L600
    1000 -> pure L1000
    1200 -> pure L1200
    1400 -> pure L1400
    1600 -> pure L1600
    1800 -> pure L1800
    2000 -> pure L2000
    2200 -> pure L2200
    2500 -> pure L2500
    _    -> J.parseFail "Invalid rating"

ratingText :: LichessRating -> Text
ratingText L600  = "600"
ratingText L1000 = "1000"
ratingText L1200 = "1200"
ratingText L1400 = "1400"
ratingText L1600 = "1600"
ratingText L1800 = "1800"
ratingText L2000 = "2000"
ratingText L2200 = "2200"
ratingText L2500 = "2500"

data LichessParams
  = LichessParams
  { _lichessRatings :: Vector LichessRating
  , _lichessSpeeds  :: Vector GameSpeed
  , _universals :: UniversalParams
  } deriving (Eq, Show)
makeLenses ''LichessParams

type PlayerName = Text

data PlayerParams
  = PlayerParams
  { _playerColor      :: Color
  , _player           :: !PlayerName
  , _playerSpeeds     :: Vector GameSpeed
  , _playerUniversals :: UniversalParams
  , _playerSince      :: (Year, MonthOfYear) -- default to Jan 1952
  } deriving (Eq, Show)
makeLenses ''PlayerParams

data RawStatsMove
  = RawStatsMove
  { _rawWhite :: Int
  , _rawBlack :: Int
  , _rawDraw  :: Int
  , _rawUci   :: Text
  } deriving (Eq, Show)
makeLenses ''RawStatsMove

instance FromJSON RawStatsMove where
  parseJSON = J.withObject "RawStatsMove" $ \v ->
    RawStatsMove
      <$> v .: "white"
      <*> v .: "black"
      <*> v .: "draws"
      <*> v .: "uci"

data RawStats
  = RawStats
  { _rawTotal      :: Int
  , _whiteTotal    :: Int
  , _blackTotal    :: Int
  , _rawStatsMoves :: [RawStatsMove]
  } deriving (Eq, Show)
makeLenses ''RawStats

instance FromJSON RawStats where
  parseJSON = J.withObject "RawStats" $ \v -> do
    white <- v .: "white"
    black <- v .: "black"
    draws <- v .: "draws"
    moves <- v .: "moves"
    statsMoves <- traverse parseJSON moves
    pure $ RawStats (white + black + draws) white black statsMoves


