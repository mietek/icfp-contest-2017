module Main where

import Control.Monad (forever)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Network.Socket as N
import System.Exit (exitSuccess)
import System.IO (Handle, IOMode(..), hPutStrLn, stderr, stdin, stdout)
import System.Random (Random, randomRIO)

import Options
import Definitions
import ClientState
import Protocol


-- NOTE: This assumes no message is smaller than 9:{"x":"y"}
getSizedMessage :: Handle -> IO BS.ByteString
getSizedMessage hdl = do
  firstPart <- BS.hGet hdl 11
  let (lengthPart, otherPart) = BS.break ((==) ':') firstPart
      totalLength = read (BS.unpack lengthPart)
      remainingLength = totalLength - (BS.length otherPart - 1)
  remainingPart <- BS.hGet hdl remainingLength
  return (BS.append (BS.tail otherPart) remainingPart)

putSizedMessage :: Handle -> LBS.ByteString -> IO ()
putSizedMessage hdl str = do
  let totalLength = LBS.pack (show (LBS.length str))
  LBS.hPutStr hdl (LBS.concat [totalLength, ":", str])


assertJust :: Maybe a -> String -> IO a
assertJust Nothing  msg = error msg
assertJust (Just x) _   = return x

assertNothing :: Maybe a -> String -> IO ()
assertNothing Nothing  _   = return ()
assertNothing (Just _) msg = error msg

assertHead :: [a] -> String -> IO a
assertHead []      msg = error msg
assertHead (x : _) _   = return x


getServerMessage :: Handle -> IO ServerMessage
getServerMessage hdl = do
  str <- getSizedMessage hdl
  assertJust (decodeServerMessage str) $
    "invalid server message: " ++ BS.unpack str

putClientMessage :: Handle -> ClientMessage -> IO ()
putClientMessage hdl msg =
  putSizedMessage hdl (encodeClientMessage msg)


note :: String -> IO ()
note = hPutStrLn stderr


randomValidRIO :: (Random a) => (a -> Bool) -> Int -> (a, a) -> IO (Maybe a)
randomValidRIO isValid maxTries range = loop 0
  where
    loop tries
      | tries == maxTries = return Nothing
      | otherwise = do
        x <- randomRIO range
        if isValid x
          then return (Just x)
          else loop (tries + 1)


pass :: ClientState -> IO Move
pass ClientState{..} =
  return Pass
    { pPunter = csPunterId
    }


isMoveValid :: ClientState -> Move -> Bool
isMoveValid _               Pass{..}  = True
isMoveValid ClientState{..} Claim{..} =
  case lookupSite csSiteMap cSource of
    Nothing -> False
    Just SiteInfo{..} ->
      if not (memberSite siNeighbours cTarget)
        then False
        else lookupClaim csClaimMap (riverId cSource cTarget) == Nothing

randomClaim :: ClientState -> IO Move
randomClaim ClientState{..} = do
  let maxSid = siteCount csSiteMap - 1
  sid <- randomRIO (0, maxSid)
  tid <- randomRIO (0, maxSid)
  return Claim
    { cPunter = csPunterId
    , cSource = sid
    , cTarget = tid
    }

randomValidMove :: ClientState -> IO Move
randomValidMove cs = loop 0
  where
    loop :: Int -> IO Move
    loop 1000 = pass cs
    loop n    = do
      move <- randomClaim cs
      if isMoveValid cs move
        then return move
        else loop (n + 1)


makeMove :: ClientState -> IO (Move, ClientState)
makeMove cs@ClientState{..} = do
  --
  -- TODO: Do something useful here
  --
  move <- randomValidMove cs
  return (move, cs)


data ClientResponse =
    Reply ClientMessage
  | Wait
  | Exit
  deriving (Eq, Ord, Show)

handleOfflineServerMessage :: ServerMessage -> IO ClientResponse
handleOfflineServerMessage msg =
  case msg of
    HandshakeReply{..} ->
      return Wait
    SetupQuery{..} -> do
      let siteMap = fullSiteMap sqSites sqRivers sqMines
          cs = emptyClientState
            { csPunterId    = sqPunter
            , csPunterCount = sqPunters
            , csSiteMap     = siteMap
            }
      note $ "Site map: " ++ show siteMap
      return $ Reply $ SetupReply
        { srReady = sqPunter
        , srState = Just cs
        }
    GameplayQuery{..} -> do
      cs@ClientState{..} <- assertJust gqState "missing state in gameplay query"
      let newClaimMap = insertMoves csClaimMap gqMoves
          csBeforeMove = cs { csClaimMap = newClaimMap }
      note $ "Claim map: " ++ show newClaimMap
      (move, csAfterMove) <- makeMove csBeforeMove
      return $ Reply $ GameplayReply
        { grMove  = move
        , grState = Just csAfterMove
        }
    ScoringNotice{..} -> do
      ClientState{..} <- assertJust snState "missing state in scoring notice"
      note $ "Claim map: " ++ show csClaimMap
      note $ "Scores: " ++ show snScores
      return Exit
    TimeoutNotice{..} ->
      return Wait

handleOnlineServerMessage :: Maybe ClientState -> ServerMessage -> IO (ClientResponse, Maybe ClientState)
handleOnlineServerMessage Nothing msg = do
  note $ "\nReceived: " ++ show msg
  case msg of
    hr@HandshakeReply{..} -> do
      Wait <- handleOfflineServerMessage hr
      return (Wait, Nothing)
    sq@SetupQuery{..} -> do
      Reply sr@SetupReply{..} <- handleOfflineServerMessage sq
      return (Reply sr { srState = Nothing }, srState)
    _ ->
      error "unexpected server message in stateless phase"
handleOnlineServerMessage jcs@(Just ClientState{..}) msg = do
  note $ "\nReceived: " ++ show msg
  case msg of
    gq@GameplayQuery{..} | gqState == Nothing -> do
      Reply gr@GameplayReply{..} <- handleOfflineServerMessage (gq { gqState = jcs })
      return (Reply gr { grState = Nothing }, grState)
    sn@ScoringNotice{..} | snState == Nothing -> do
      Exit <- handleOfflineServerMessage (sn { snState = jcs })
      return (Exit, jcs)
    tn@TimeoutNotice{..} -> do
      Wait <- handleOfflineServerMessage tn
      return (Wait, jcs)
    _ ->
      error "unexpected server message in stateful phase"

performClientResponse :: Handle -> ClientResponse -> IO ()
performClientResponse output response =
  case response of
    Reply reply -> do
      note $ "Replying: " ++ show reply
      putClientMessage output reply
    Wait ->
      note "Waiting..."
    Exit -> do
      note "Exiting..."
      exitSuccess


offlineMain :: String -> IO ()
offlineMain punterName = do
  note "Offline mode"
  putClientMessage stdout (HandshakeQuery { hqMe = punterName })
  forever $ do
    msg <- getServerMessage stdin
    response <- handleOfflineServerMessage msg
    performClientResponse stdout response


connectToServer :: String -> Int -> IO Handle
connectToServer host port = do
  addresses <- N.getAddrInfo Nothing (Just host) (Just (show port))
  address <- assertHead addresses "could not resolve server address"
  socket <- N.socket (N.addrFamily address) N.Stream N.defaultProtocol
  N.connect socket (N.addrAddress address)
  hdl <- N.socketToHandle socket ReadWriteMode
  return hdl

onlineMain :: String -> Int -> String -> IO ()
onlineMain serverHost serverPort punterName = do
  note $ "Online mode (" ++ serverHost ++ ":" ++ show serverPort ++ ")"
  N.withSocketsDo $ do
    note "Connecting..."
    hdl <- connectToServer serverHost serverPort
    note "Connected"
    putClientMessage hdl (HandshakeQuery { hqMe = punterName })
    loop hdl Nothing
  where
    loop :: Handle -> Maybe ClientState -> IO ()
    loop hdl cs = do
      msg <- getServerMessage hdl
      (response, newCS) <- handleOnlineServerMessage cs msg
      performClientResponse hdl response
      loop hdl newCS


main :: IO ()
main = do
  Options{..} <- getOptions
  note oPunterName
  if oOnlineMode
    then onlineMain oServerHost oServerPort oPunterName
    else offlineMain oPunterName
