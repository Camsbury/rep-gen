{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeFamilies #-}

module PyChess where

import Prelude
import Foreign.C.String
import Foreign.C.Types
import Data.Aeson
  ( FromJSON(..)
  , Value(..)
  , (.:)
  , decode
  )
import Data.Aeson.Types (parseFail)

foreign import ccall "ucis_to_fen" ucis_to_fen
  :: CString -> IO CString
foreign import ccall "sans_to_ucis" sans_to_ucis
  :: CString -> IO CString
foreign import ccall "ucis_to_engine_candidates" ucis_to_engine_candidates
  :: CString -> CString -> Int -> Int -> IO CString

data Score
  = Cp   Int
  | Mate Int
  deriving (Show, Eq)

-- | Parses a Stockfish representation of a score into ours
parseScore :: Text -> Maybe Score
parseScore = undefined

instance FromJSON Score where
  parseJSON (String s)
    = maybe (parseFail "Score string not properly formatted") pure
    $ parseScore s
  parseJSON _ = parseFail "Score not provided as a JSON string"

data UciAndScore
  = UciAndScore
  { uci   :: !Text
  , score :: Score
  } deriving (Show, Eq)

instance FromJSON UciAndScore where
  parseJSON (Object v) =
    UciAndScore
      <$> v .: "uci"
      <*> v .: "score"
  parseJSON _ = parseFail "UciAndScore not provided as a JSON object"


-- | Convert a sequence of UCI 'Text' into a FEN 'Text'
ucisToFen :: (MonoFoldable m, Element m ~ Text) => m -> IO Text
ucisToFen ucis = do
  cUcis <- newCString . unpack $ intercalate "," ucis
  result <- ucis_to_fen cUcis
  pack <$> peekCString result

-- TODO: Split the result into a Vector (or maybe the same form as the input)
-- | Convert a sequence of SAN 'Text' into UCI 'Text'
sansToUcis :: (MonoFoldable m, Element m ~ Text) => m -> IO Text
sansToUcis sans = do
  cSans <- newCString . unpack $ intercalate "," sans
  result <- sans_to_ucis cSans
  pack <$> peekCString result

data Color
  = White
  | Black
  deriving (Show, Eq)

-- | Convert a 'Color' to 'Text'
colorText :: Color -> Text
colorText c
  | c == White = "white"
  | c == Black = "black"

-- | Convert a sequence of UCI 'Text' into a Vector/sequence of 'UciAndScore'
-- representing the candidate moves from the position
ucisToEngineCandidates
  :: (MonoFoldable m, Element m ~ Text)
  => m
  -> Color
  -> Int
  -> Int
  -> IO (Maybe (Vector UciAndScore))
ucisToEngineCandidates ucis color depth moveCount = do
  cUcis <- newCString . unpack $ intercalate "," ucis
  cColor <- newCString . unpack . colorText $ color
  cResult <- ucis_to_engine_candidates cUcis cColor depth moveCount
  result <- peekCString cResult
  pure . decode $ fromString result

