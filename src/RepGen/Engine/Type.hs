{-# LANGUAGE TemplateHaskell #-}
--------------------------------------------------------------------------------
module RepGen.Engine.Type
  ( module RepGen.Engine.Type
  ) where
--------------------------------------------------------------------------------
import Data.Aeson
  ( FromJSON(..)
  , Value(..)
  , (.:)
  , (.:?)
  )
import RepGen.Type
import RepGen.Score.Type
--------------------------------------------------------------------------------
import qualified Data.Aeson as J
import qualified Data.Aeson.Types as J
import qualified RepGen.Score as S
--------------------------------------------------------------------------------

data EngineCandidate
  = EngineCandidate
  { _ngnUci :: !Uci
  , _ngnScore  :: Score
  } deriving (Show, Eq)
makeLenses ''EngineCandidate

instance FromJSON EngineCandidate where
  parseJSON (Object v) =
    EngineCandidate
      <$> v .: "uci"
      <*> v .: "score"
  parseJSON _ = J.parseFail "EngineCandidate not provided as a JSON object"

engineToMap :: Iso' [EngineCandidate] (Map Uci Score)
engineToMap = iso
  (mapFromList . fmap engineToEntry)
  (fmap entryToEngine . mapToList)
  where
    engineToEntry ngn = (ngn ^. ngnUci, ngn ^. ngnScore)
    entryToEngine (uci, score) = EngineCandidate uci score

newtype CloudEngineCandidates
  = CloudEngineCandidates
  { _cloudCands :: [EngineCandidate]
  } deriving (Show, Eq)
makeLenses ''CloudEngineCandidates

instance FromJSON CloudEngineCandidates where
  parseJSON = J.withObject "CloudEngineCandidates" $ \o -> do
    pvs <- o .: "pvs"
    CloudEngineCandidates <$> traverse parsePV pvs
    where
      parsePV pv = do
        moves <- pv .: "moves"
        case words moves of
          [] -> J.parseFail "No moves in the moves string"
          (move : _) -> do
            cp <- pv .:? "cp"
            case cp of
              Just n -> pure . EngineCandidate move . S.fromRaw $ CpScore n
              Nothing -> do
                mate <- pv .: "mate"
                case mate of
                  Number n -> do
                    let color = if n < 0 then Black else White
                    pure
                      . EngineCandidate move
                      . S.fromRaw
                      . MateScore
                      . MateIn color
                      . abs
                      $ round n
                  _ -> J.parseFail "Expected a number for mate"
