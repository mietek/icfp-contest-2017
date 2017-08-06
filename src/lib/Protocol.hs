module Protocol where

import qualified Data.Aeson as JSON
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Aeson (FromJSON, ToJSON, parseJSON, toJSON, (.:), (.:?), (.=))
import Data.Foldable (asum)
import Data.Monoid ((<>))
import GHC.Exts (fromList)

import Definitions
import ClientState


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
    JSON.object
      [ "me"    .= hqMe
      ]
  toJSON (SetupReply{..}) =
    JSON.object $ filterNull
      [ "ready" .= srReady
      , "state" .= srState
      ]
  toJSON (GameplayReply{..}) =
    JSON.Object $
         toObject grMove
      <> fromList (filterNull
           [ "state" .= grState
           ])

encodeClientMessage :: ClientMessage -> LBS.ByteString
encodeClientMessage = JSON.encode


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
  | ScoringNotice
      { snMoves   :: [Move]
      , snScores  :: [Score]
      , snState   :: Maybe ClientState
      }
  | TimeoutNotice
      { tnTimeout :: Float
      }
  deriving (Eq, Ord, Show)

instance FromJSON ServerMessage where
  parseJSON =
    JSON.withObject "serverMessage" $ \o -> asum
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
          snMoves   <- stopO .: "moves"
          snScores  <- stopO .: "scores"
          snState   <- o .:? "state"
          return ScoringNotice{..}
      , do
          tnTimeout <- o .: "timeout"
          return TimeoutNotice{..}
      ]

decodeServerMessage :: BS.ByteString -> Maybe ServerMessage
decodeServerMessage = JSON.decodeStrict'


toObject :: ToJSON a => a -> JSON.Object
toObject x =
  case toJSON x of
    JSON.Object o -> o
    _             -> error "toObject: unexpected value"

filterNull :: [(a, JSON.Value)] -> [(a, JSON.Value)]
filterNull =
    filter ((/=) JSON.Null . snd)
