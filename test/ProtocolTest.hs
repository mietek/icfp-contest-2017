module ProtocolTest where

import qualified Data.Aeson as J
import qualified Data.ByteString.Lazy as LBS
import Test.Tasty
import Test.Tasty.HUnit

import Definitions
import ClientState
import Protocol


encode source json =
  J.encode source @?= json

decode json target =
  J.decode json @?= Just target


emptyClientStateJSON =
  "{\"punterCount\":0,\"siteMap\":[],\"punterId\":0,\"claimMap\":[]}"


unit_encodeClientState =
  encode
    emptyClientState
    emptyClientStateJSON
    
unit_encodeClaim =
  encode
    (Claim
      { cPunter = 0
      , cSource = 0
      , cTarget = 1
      })
    "{\"claim\":{\"punter\":0,\"source\":0,\"target\":1}}"

unit_encodePass =
  encode
    (Pass 
      { pPunter = 0
      })
    "{\"pass\":{\"punter\":0}}"
    
unit_encodeHandshakeQuery =
  encode
    (HandshakeQuery
      { hqMe = "fnord"
      })
    "{\"me\":\"fnord\"}"
    
unit_encodeOnlineSetupReply =
  encode 
    (SetupReply
      { srReady = 0
      , srState = Nothing
      })
    "{\"ready\":0}"
    
unit_encodeOnlineGameplayReply =
  encode
    (GameplayReply
      { grMove  = Pass { pPunter = 0 }
      , grState = Nothing
      })
    "{\"pass\":{\"punter\":0}}"
    
unit_encodeOfflineSetupReply =
  encode
    (SetupReply
      { srReady = 0
      , srState = Just emptyClientState
      })
    (LBS.concat ["{\"state\":", emptyClientStateJSON, ",\"ready\":0}"])
    
unit_encodeOfflineGameplayReply =
  encode
    (GameplayReply
      { grMove  = Pass { pPunter = 0 }
      , grState = Just emptyClientState
      })
    (LBS.concat ["{\"state\":", emptyClientStateJSON, ",\"pass\":{\"punter\":0}}"])

unit_decodeClientState =
  decode
    emptyClientStateJSON
    emptyClientState

unit_decodeRiver =
  decode
    "{ \"source\": 0\
    \, \"target\": 1\
    \}"
    (River
      { rSource = 0
      , rTarget = 1
      })

unit_decodeClaim =
  decode 
    "{ \"claim\":\
    \  { \"punter\": 0\
    \  , \"source\": 0\
    \  , \"target\": 1\
    \  }\
    \}"
    (Claim
      { cPunter = 0
      , cSource = 0
      , cTarget = 1
      })

unit_decodePass =
  decode 
    "{ \"pass\":\
    \  { \"punter\": 0\
    \  }\
    \}"
    (Pass 
      { pPunter = 0
      })
      
unit_decodeScore =
  decode 
    "{ \"punter\": 0\
    \, \"score\":  999999999\
    \}"
  (Score
    { sPunter = 0
    , sScore  = 999999999
    })

unit_decodeHandshakeReply =
  decode 
    "{ \"you\": \"fnord\"\
    \}"
    (HandshakeReply
      { hrYou = "fnord"
      })

unit_decodeSetupQuery =
  decode 
    "{ \"punter\":  0\
    \, \"punters\": 2\
    \, \"map\":\
    \    { \"sites\":  [0, 1]\
    \    , \"rivers\": [{\"source\": 0, \"target\": 1}]\
    \    , \"mines\":  [0]\
    \    }\
    \}"
    (SetupQuery
      { sqPunter  = 0
      , sqPunters = 2
      , sqSites   = [0, 1]
      , sqRivers  = [River { rSource = 0, rTarget = 1 }]
      , sqMines   = [0]
      })

unit_decodeOnlineGameplayQuery =
  decode 
    "{ \"move\":\
    \  { \"moves\":\
    \    [ { \"claim\": { \"punter\": 0, \"source\": 1, \"target\": 0 } }\
    \    , { \"pass\":  { \"punter\": 1 } }\
    \    ]\
    \  }\
    \}"
    (GameplayQuery
      { gqMoves =
          [ Claim { cPunter = 0, cSource = 1, cTarget = 0 }
          , Pass { pPunter = 1 }
          ]
      , gqState = Nothing
      })

unit_decodeOnlineScoringNotice =
  decode
    "{ \"stop\":\
    \  { \"moves\":\
    \    [ { \"claim\": { \"punter\": 1, \"source\": 0, \"target\": 1 } }\
    \    , { \"pass\":  { \"punter\": 0 } }\
    \    ]\
    \  , \"scores\": [ { \"punter\": 1, \"score\": 999999999 }]\
    \  }\
    \}"
    (ScoringNotice
      { snMoves  =
          [ Claim { cPunter = 1, cSource = 0, cTarget = 1 }
          , Pass { pPunter = 0 }
          ]
      , snScores = [Score { sPunter = 1, sScore = 999999999 }]
      , snState  = Nothing
      })

unit_decodeOfflineGameplayQuery =
  decode 
    (LBS.concat [
      "{ \"move\":\
      \  { \"moves\":\
      \    [ { \"claim\": { \"punter\": 1, \"source\": 0, \"target\": 1 } }\
      \    , { \"pass\":  { \"punter\": 0 } }\
      \    ]\
      \  }\
      \, \"state\":", emptyClientStateJSON, "}"])
    (GameplayQuery
      { gqMoves =
          [ Claim { cPunter = 1, cSource = 0, cTarget = 1 }
          , Pass { pPunter = 0 }
          ]
      , gqState = Just emptyClientState
      })

unit_decodeOfflineScoringNotice =
  decode 
    (LBS.concat [
      "{ \"stop\":\
      \  { \"moves\":\
      \    [ { \"claim\": { \"punter\": 0, \"source\": 1, \"target\": 0 } }\
      \    , { \"pass\":  { \"punter\": 1 } }\
      \    ]\
      \  , \"scores\": [ { \"punter\": 0, \"score\": 999999999 }]\
      \  }\
      \, \"state\":", emptyClientStateJSON, "}"])
    (ScoringNotice
      { snMoves  =
          [ Claim { cPunter = 0, cSource = 1, cTarget = 0 }
          , Pass { pPunter = 1 }
          ]
      , snScores = [Score { sPunter = 0, sScore = 999999999 }]
      , snState  = Just emptyClientState
      })
