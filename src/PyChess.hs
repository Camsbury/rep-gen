{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeFamilies #-}

module PyChess
  ( ucisToFen
  , sansToUcis
  , ucisToEngineCandidates
  ) where

import Prelude
import Foreign.C.String
import Foreign.C.Types
import Control.Lens.Operators
import Control.Lens.Combinators
import Data.Aeson
  ( FromJSON(..)
  , Value(..)
  , (.:)
  , decode
  )
import Data.Aeson.Types (parseFail)
import Text.Regex.PCRE ((=~))

foreign import ccall "ucis_to_fen" ucis_to_fen
  :: CString -> IO CString
foreign import ccall "sans_to_ucis" sans_to_ucis
  :: CString -> IO CString
foreign import ccall "ucis_to_engine_candidates" ucis_to_engine_candidates
  :: CString -> Int -> Int -> IO CString

data Color
  = White
  | Black
  deriving (Show, Eq)

data MateIn
  = MateIn
  { color     :: Color
  , moveCount :: Int
  } deriving (Show, Eq)

type Cp = Int

data Score
  = CpScore   Cp
  | MateScore MateIn
  deriving (Show, Eq)

instance FromJSON Score where
  parseJSON (String s)
    = maybe (parseFail "Score string not properly formatted") pure
    $ parseScore s
  parseJSON _ = parseFail "Score not provided as a JSON string"

data EngineCandidate
  = EngineCandidate
  { uci   :: !Text
  , score :: Score
  } deriving (Show, Eq)

instance FromJSON EngineCandidate where
  parseJSON (Object v) =
    EngineCandidate
      <$> v .: "uci"
      <*> v .: "score"
  parseJSON _ = parseFail "EngineCandidate not provided as a JSON object"

parseMateIn :: Text -> Maybe Score
parseMateIn text = do
  let p :: String = "\\#([+-])(\\d+)"
  case (unpack text =~ p :: (String, String, String, [String])) ^. _4 of
    ["+", n]
      -> Just
      . MateScore
      $ MateIn
      { color = White
      , moveCount = parseCount n
      }

    ["-", n]
      -> Just
      . MateScore
      $ MateIn
      { color = Black
      , moveCount = parseCount n
      }

    _ -> Nothing
  where
    parseCount :: String -> Int
    parseCount = fromMaybe (error "impossible regex") . readMay

parseCp :: Text -> Maybe Score
parseCp text = do
  let p :: String = "\\+{0,1}(-{0,1}\\d+)"
  case (unpack text =~ p :: (String, String, String, [String])) ^. _4 of
    [n] -> CpScore <$> readMay n
    _ -> Nothing

-- | Parses a Stockfish representation of a score into ours
parseScore :: Text -> Maybe Score
parseScore text = parseMateIn text <|> parseCp text

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

-- | Convert a sequence of UCI 'Text' into a Vector/sequence of 'EngineCandidate'
ucisToEngineCandidates
  :: (MonoFoldable m, Element m ~ Text)
  => m
  -> Int
  -> Int
  -> IO (Maybe (Vector EngineCandidate))
ucisToEngineCandidates ucis depth moveCount = do
  cUcis <- newCString . unpack $ intercalate "," ucis
  cResult <- ucis_to_engine_candidates cUcis depth moveCount
  result <- peekCString cResult
  pure . decode $ fromString result

