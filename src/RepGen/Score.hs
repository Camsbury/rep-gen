{-# OPTIONS_GHC -Wno-orphans #-}
--------------------------------------------------------------------------------
-- |
-- Module: RepGen.Score
--
-- NOTE: want to experiment with wdl (avail in python-chess)
-- stockfish wdl is calculated by the following:
-- https://github.com/official-stockfish/Stockfish/blob/master/src/uci.cpp#L201

-- NOTE: helpful links
-- https://lichess.org/page/accuracy
-- https://github.com/lichess-org/lila/blob/master/modules/analyse/src/main/WinPercent.scala#L23
--
--------------------------------------------------------------------------------
module RepGen.Score where
--------------------------------------------------------------------------------
import RepGen.Type
import RepGen.Score.Type
--------------------------------------------------------------------------------
import Data.Aeson
  ( FromJSON(..)
  , Value(..)
  )
import Data.Aeson.Types (parseFail)
import Text.Regex.PCRE ((=~))
--------------------------------------------------------------------------------

instance FromJSON Score where
  parseJSON (String s)
    = maybe (parseFail "Score string not properly formatted") pure
    $ parseScore s
  parseJSON _ = parseFail "Score not provided as a JSON string"

-- | Parses a Stockfish representation of a score into ours
parseScore :: Text -> Maybe Score
parseScore text = fmap fromRaw $ parseMateIn text <|> parseCp text

-- | Found empirically, referenced in module docs
scoreConstant :: Double
scoreConstant = -0.00368208

-- | Used to standardize scores into a single number for comparison of moves
-- similar to something like wdl
fromRaw :: RawScore -> Score
fromRaw (MateScore (MateIn White c))
  = Score . succ $ 1 /. c
fromRaw (MateScore (MateIn Black c))
  = Score . negate . succ $ 1 /. c
fromRaw (CpScore cp)
  = Score
  . (+0.5)
  . (*0.5)
  . pred
  . (2/)
  . succ
  . exp
  . (*scoreConstant)
  $ fromIntegral cp

-- | Used to parse scores for "mate in x"
parseMateIn :: Text -> Maybe RawScore
parseMateIn text = do
  let p :: String = "\\#([+-])(\\d+)"
  case (unpack text =~ p :: (String, String, String, [String])) ^. _4 of
    ["+", n]
      -> Just
      . MateScore
      $ MateIn
      { _miColor = White
      , _moveCount = parseCount n
      }

    ["-", n]
      -> Just
      . MateScore
      $ MateIn
      { _miColor = Black
      , _moveCount = parseCount n
      }

    _ -> Nothing
  where
    parseCount :: String -> Int
    parseCount = fromMaybe (error "impossible regex") . readMay

-- | Used to parse centipawn scores
parseCp :: Text -> Maybe RawScore
parseCp text = do
  let p :: String = "\\+{0,1}(-{0,1}\\d+)"
  case (unpack text =~ p :: (String, String, String, [String])) ^. _4 of
    [n] -> CpScore <$> readMay n
    _ -> Nothing
