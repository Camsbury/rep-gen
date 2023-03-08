--------------------------------------------------------------------------------
module Main where
--------------------------------------------------------------------------------
import RepGen
import Options.Applicative
  ( (<**>)
  , Parser
  )
--------------------------------------------------------------------------------
import qualified Options.Applicative as O
import qualified Data.Aeson as J
--------------------------------------------------------------------------------

main :: IO ()
main
  = buildRepertoire
  . first pack
  . J.eitherDecode
  . fromStrict
  =<< O.customExecParser (O.prefs O.showHelpOnEmpty) opts
  -- buildRepertoire $
  --   def & colorL   .~ Black
  --       & mastersP .~ False
  --       -- & minProbAgg .~ 0.01
  --       -- & initRespProb .~ 0.5
  --       -- & asymRespProb .~ 0.9

  --       -- & startingMoves .~ ["Nf3"]
  --       -- & minLogLevel .~ LevelDebug


  --       -- & minLogLevel .~ LevelDebug
  --       & mOverrides .~ mapFromList ([(["e4"], "c5")] :: [([San], San)])
  --       -- & startingMoves .~ ["f4"]
  where
    opts = O.info (parseRepGenConfig <**> O.helper) desc
    desc = O.fullDesc
         <> O.progDesc "Builds a repertoire based on a passed \
                      \ configuration. Can include masters' games \
                      \ in addition to the default lichess games \
                      \ filtered by speed and rating. Decorated by \
                      \ engine scores for filtering. Intended to be \
                    \ concrete use cases soon."
         <> O.header "rep-gen - A chess repertoire generator"

parseRepGenConfig :: Parser ByteString
parseRepGenConfig
  = O.strOption
  ( O.long "config"
  <> O.short 'c'
  <> O.metavar "CONFIG"
  <> O.value "{}"
  <> O.help "Read config from CONFIG"
  )
