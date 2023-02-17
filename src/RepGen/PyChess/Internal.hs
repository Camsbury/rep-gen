{-# OPTIONS_GHC -Wno-orphans #-}
module RepGen.PyChess.Internal where



import Data.Aeson
  ( FromJSON(..)
  , Value(..)
  , (.:)
  )
import Data.Aeson.Types (parseFail)
import RepGen.Type
import RepGen.PyChess.Type
import Text.Regex.PCRE ((=~))

--------------------------------------------------------------------------------
--- Impl
--------------------------------------------------------------------------------

instance FromJSON Score where
  parseJSON (String s)
    = maybe (parseFail "Score string not properly formatted") pure
    $ parseScore s
  parseJSON _ = parseFail "Score not provided as a JSON string"

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
      { _color = White
      , _moveCount = parseCount n
      }

    ["-", n]
      -> Just
      . MateScore
      $ MateIn
      { _color = Black
      , _moveCount = parseCount n
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
