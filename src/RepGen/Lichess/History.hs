--------------------------------------------------------------------------------
module RepGen.Lichess.History
  ( module RepGen.Lichess.History.Type
  , lichessMoves
  , maybeMastersMoves
  , initialStats
  ) where
--------------------------------------------------------------------------------
import RepGen.Config.Type
import RepGen.Lichess.History.Type
import RepGen.Stats
import RepGen.Stats.Type
import RepGen.Type
--------------------------------------------------------------------------------
import qualified Control.Concurrent as C
import qualified Data.Aeson as J
import qualified Web
--------------------------------------------------------------------------------

baseUrl :: Text
baseUrl = "https://explorer.lichess.ovh/"

-- | One minute in microseconds
oneMinute :: Int
oneMinute = 60 * 1000 * 1000

--------------------------------------------------------------------------------

lichessMoves
  :: ( MonadReader RGConfig m
    , MonadLogger m
    , MonadError  RGError  m
    , MonadIO m
    )
  => Fen
  -> m [(Uci, NodeStats)]
lichessMoves = fmap parseStats . historicMoves <=< getLichessParams

-- | Fetches masters moves if they meet our criteria
maybeMastersMoves
  :: ( MonadReader RGConfig m
    , MonadLogger m
    , MonadError  RGError  m
    , MonadIO m
    )
  => Fen
  -> m (Maybe [(Uci, NodeStats)])
maybeMastersMoves fen = do
  useM <- view mastersP
  mps <- getMastersParams fen
  if useM
    then fmap (fmap parseStats) . filterMinTotal =<< historicMoves mps
    else pure Nothing

initialStats
  :: ( MonadReader RGConfig m
    , MonadLogger m
    , MonadError  RGError  m
    , MonadIO m
    )
  => m RGStats
initialStats = do
  lcStats    <- fmap rawToNode . historicMoves =<< getLichessParams def
  mStats     <- fmap rawToNode . historicMoves =<< getMastersParams def
  useMasters <- view mastersP
  pure $ def
       & lichessStats ?~ lcStats
       & mastersStats .~ bool Nothing (Just mStats) useMasters
       & probAgg .~ lcStats ^. prob

class HistoricFetchable a where
  historicMoves
    :: ( MonadReader RGConfig m
      , MonadLogger m
      , MonadError  RGError  m
      , MonadIO m
      )
    => a
    -> m RawStats

instance HistoricFetchable UniversalParams where
  historicMoves params
    = fetchMovesFor (fromMastersParams params) "masters"

-- | Convert masters params to query params
fromMastersParams :: UniversalParams -> Map Text Text
fromMastersParams params
  = mapFromList
  [ ("moves",    params ^. moveCount . to tshow)
  , ("fen",      params ^. fenParam . fenL)
  , ("topGames", "0")
  ]

instance HistoricFetchable LichessParams where
  historicMoves params
    = fetchMovesFor (fromLichessParams params) "lichess"

-- | Convert lichess params to query params
fromLichessParams :: LichessParams -> Map Text Text
fromLichessParams params
  = mapFromList
  [ ( "ratings"
    ,  params ^. lichessRatings . to (intercalate "," . fmap ratingText)
    )
  , ( "speeds"
    , params ^. lichessSpeeds . to (intercalate "," . fmap speedText)
    )
  , ("recentGames", "0")
  ] <> fromMastersParams (params ^. universals)

-- | get historic Lichess moves for masters games
fetchMovesFor
  :: ( MonadReader RGConfig m
    , MonadLogger m
    , MonadError  RGError  m
    , MonadIO m
    )
  => Map Text Text
  -> Text
  -> m RawStats
fetchMovesFor params path = do
  dbPath <- view httpCachePath
  (statusCode, response)
    <- liftIO
    . Web.cachedGetRequest dbPath (baseUrl <> path) False
    $ params
  case statusCode of
    200 -> throwEither
        . first pack
        . J.eitherDecode
        . fromStrict
        $ response
    429 -> do
      liftIO $ C.threadDelay oneMinute
      fetchMovesFor params path
    404 -> throwError
        ( "404 Not found for params: "
        <> tshow params
        )
    code -> throwError
         ( "HTTP error code: "
         <> tshow code
         <> " for params: "
         <> tshow params
         )

filterMinTotal
  :: (MonadReader RGConfig m, MonadIO m)
  => RawStats
  -> m (Maybe RawStats)
filterMinTotal rs = do
  mtm <- view minTotalMasters
  pure $ if rs ^. rawTotal > mtm
    then Just rs
    else Nothing

getLichessParams
  :: (MonadReader RGConfig m, MonadIO m)
  => Fen
  -> m LichessParams
getLichessParams fen = do
  hmc <- view $ historyConfig . historyMoveCount
  speeds <- view $ historyConfig . historySpeeds
  ratings <- view $ historyConfig . historyRatings
  let ups = UniversalParams hmc fen
  pure $ LichessParams ratings speeds ups

getMastersParams
  :: (MonadReader RGConfig m, MonadIO m)
  => Fen
  -> m UniversalParams
getMastersParams fen = do
  hmc <- view $ historyConfig . historyMoveCount
  pure $ UniversalParams hmc fen

rawToNode :: RawStats -> NodeStats
rawToNode rs = NodeStats whiteS blackS 1 total
  where
    white = rs ^. whiteTotal
    black = rs ^. blackTotal
    total = rs ^. rawTotal
    whiteS = mkRGStat $ white /. total
    blackS = mkRGStat $ black /. total
