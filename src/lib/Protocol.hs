module Protocol where

import qualified Data.Aeson as J
import Data.Aeson (FromJSON, ToJSON, parseJSON, toJSON, (.:), (.:?), (.=))
import Data.Foldable (asum)
import Data.Monoid ((<>))
import GHC.Exts (fromList)
        
        
data ClientState = ClientState
  { csDummy :: Int
  }
  deriving (Eq, Ord, Show)
  
instance FromJSON ClientState where
  parseJSON =
    J.withObject "clientState" $ \o -> do
      csDummy <- o .: "dummy"
      return ClientState{..}
  
instance ToJSON ClientState where
  toJSON ClientState{..} =
    J.object
      [ "dummy" .= csDummy
      ]


type PunterId = Int

type SiteId = Int


data River = River
  { rSource :: SiteId
  , rTarget :: SiteId
  }
  deriving (Eq, Ord, Show)

instance FromJSON River where
  parseJSON =
    J.withObject "river" $ \o -> do
      rSource <- o .: "source"
      rTarget <- o .: "target"
      return River{..}


data Move =
    Claim
      { cPunter :: PunterId
      , cSource :: SiteId
      , cTarget :: SiteId
      }
  | Pass
      { pPunter :: PunterId
      }
  deriving (Eq, Ord, Show)

instance FromJSON Move where
  parseJSON =
    J.withObject "move" $ \o -> asum
      [ do
          claimO  <- o .: "claim"
          cPunter <- claimO .: "punter"
          cSource <- claimO .: "source"
          cTarget <- claimO .: "target"
          return Claim{..}
      , do
          passO   <- o .: "pass"
          pPunter <- passO .: "punter"
          return Pass{..}
      ]

instance ToJSON Move where
  toJSON Claim{..} =
    J.object
      [ "claim" .=
          J.object
            [ "punter" .= cPunter
            , "source" .= cSource
            , "target" .= cTarget
            ]
      ]
  toJSON Pass{..} =
    J.object
      [ "pass" .=
          J.object
            [ "punter" .= pPunter
            ]
      ]

  
data Score = Score
  { sPunter :: PunterId
  , sScore  :: Int
  }
  deriving (Eq, Ord, Show)

instance FromJSON Score where
  parseJSON =
    J.withObject "score" $ \o -> do
      sPunter <- o .: "punter"
      sScore  <- o .: "score"
      return Score{..}


data ClientMessage =
    HandshakeQuery
      { hqMe    :: String
      }
  | SetupReply
      { srReady :: PunterId
      , srState :: Maybe ClientState
      }
  | GameplayReply
      { grMove  :: Move
      , grState :: Maybe ClientState
      }
  deriving (Eq, Ord, Show)

instance ToJSON ClientMessage where
  toJSON (HandshakeQuery{..}) =
    J.object
      [ "me"    .= hqMe
      ]
  toJSON (SetupReply{..}) =
    J.object $ filterNull
      [ "ready" .= srReady
      , "state" .= srState
      ]
  toJSON (GameplayReply{..}) =
    J.Object $
         toObject grMove
      <> fromList (filterNull
           [ "state" .= grState
           ])


data ServerMessage =
    HandshakeReply
      { hrYou     :: String
      }
  | SetupQuery
      { sqPunter  :: PunterId
      , sqPunters :: Int
      , sqSites   :: [SiteId]
      , sqRivers  :: [River]
      , sqMines   :: [SiteId]
      }
  | GameplayQuery
      { gqMoves   :: [Move]
      , gqState   :: Maybe ClientState
      }
  | ScoringQuery 
      { sqMoves   :: [Move]
      , sqScores  :: [Score]
      , sqState   :: Maybe ClientState
      }
  | TimeoutQuery
      { tqTimeout :: Float
      }
  deriving (Eq, Ord, Show)

instance FromJSON ServerMessage where
  parseJSON =
    J.withObject "serverMessage" $ \o -> asum
      [ do
          hrYou     <- o .: "you"
          return HandshakeReply{..}
      , do
          sqPunter  <- o .: "punter"
          sqPunters <- o .: "punters"
          mapO      <- o .: "map"
          sqSites   <- mapO .: "sites"
          sqRivers  <- mapO .: "rivers"
          sqMines   <- mapO .: "mines"
          return SetupQuery{..}
      , do
          moveO     <- o .: "move"
          gqMoves   <- moveO .: "moves"
          gqState   <- o .:? "state"
          return GameplayQuery{..}
      , do
          stopO     <- o .: "stop"
          sqMoves   <- stopO .: "moves"
          sqScores  <- stopO .: "scores"
          sqState   <- o .:? "state"
          return ScoringQuery{..}
      , do
          tqTimeout <- o .: "timeout"
          return TimeoutQuery{..}
      ]


toObject :: ToJSON a => a -> J.Object
toObject x =
  case toJSON x of
    J.Object o -> o
    _          -> error "toObject: value isn't an Object"

filterNull :: [(a, J.Value)] -> [(a, J.Value)]
filterNull =
    filter ((/=) J.Null . snd)
