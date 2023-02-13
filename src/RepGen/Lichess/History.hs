module RepGen.Lichess.History where

import Prelude

import RepGen.Type
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Method
import Network.HTTP.Types.URI
import Data.Time.Calendar (Year(..), MonthOfYear(..))

data GlobalParams
  = GlobalParams
  { moveCount :: Int -- moves
  , fen       :: Text
  } deriving (Eq, Show)

data GameSpeed
  = Bullet
  | Blitz
  | Rapid
  | Classical
  deriving (Eq, Show)

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

data LichessParams
  = LichessParams
  { ratings        :: Vector LichessRating
  , lichessSpeeds  :: Vector GameSpeed
  , lichessGlobals :: GlobalParams
  } deriving (Eq, Show)

type PlayerName = Text

-- TODO: add since as a date!
data PlayerParams
  = PlayerParams
  { color         :: Color
  , player        :: PlayerName
  , playerSpeeds  :: Vector GameSpeed
  , playerGlobals :: GlobalParams
  , since         :: (Year, MonthOfYear) -- default to Jan 1952
  } deriving (Eq, Show)

-- import qualified Data.ByteString.Lazy as LBS

-- makeGetRequest :: String -> [(ByteString, Maybe ByteString)] -> IO LBS.ByteString
-- makeGetRequest url queryParams = do
--   manager <- newManager tlsManagerSettings
--   request <- parseRequest url
--   let requestWithParams = setQueryString queryParams request
--   response <- httpLbs requestWithParams manager
--   pure $ responseBody response

-- main :: IO ()
-- main = do
--   let url = "https://httpbin.org/get"
--   let queryParams = [("foo", Just "bar"), ("baz", Nothing)]
--   response <- makeGetRequest url queryParams
--   print response
