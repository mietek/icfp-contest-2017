module Main where

import Control.Monad (forever)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Network.Socket as N
import System.Exit (exitSuccess)
import System.IO (Handle, IOMode(..), hPutStrLn, stderr, stdin, stdout)

import Options
import Definitions
import ClientState
import Protocol


-- NOTE: This assumes no message is smaller than 9:{"x":"y"}
getSizedMessage :: Handle -> IO BS.ByteString
getSizedMessage hdl = do
  firstPart <- BS.hGetSome hdl 11
  let (lengthPart, otherPart) = BS.break ((==) ':') firstPart
      totalLength = read (BS.unpack lengthPart)
      remainingLength = totalLength - (BS.length otherPart - 1)
  remainingPart <- BS.hGetSome hdl remainingLength
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


data ClientResponse =
    Reply ClientMessage
  | Wait
  | Exit
  deriving (Eq, Ord, Show)


noteMove :: Move -> IO ()
noteMove Claim{..} = note $ "Punter " ++ show cPunter ++ " claims " ++ show (cSource, cTarget)
noteMove Pass{..}  = note $ "Punter " ++ show pPunter ++ " passes"

noteMoves :: [Move] -> IO ()
noteMoves moves = mapM_ noteMove moves

handleOfflineServerMessage :: ServerMessage -> IO ClientResponse
handleOfflineServerMessage msg =
  case msg of
    HandshakeReply{..} ->
      return Wait
    SetupQuery{..} -> do
      let cs = emptyClientState
            { csPunterId    = sqPunter
            , csPunterCount = sqPunters
            , csSiteMap     = fullSiteMap sqSites sqRivers sqMines
            }
      return $ Reply $ SetupReply
        { srReady = sqPunter
        , srState = Just cs
        }
    GameplayQuery{..} -> do
      cs@ClientState{..} <- assertJust gqState "missing state in gameplay query"
      let newCS = cs { csClaimMap = insertMoves csClaimMap gqMoves }
      noteMoves gqMoves
      --
      -- TODO: Do something here
      --
      return $ Reply $ GameplayReply
        { grMove  = Pass { pPunter = csPunterId }
        , grState = Just newCS
        }
    ScoringNotice{..} -> do
      cs@ClientState{..} <- assertJust snState "missing state in scoring notice"
      note $ "Final state: " ++ show cs
      return Exit
    TimeoutNotice{..} ->
      return Wait

handleOnlineServerMessage :: Maybe ClientState -> ServerMessage -> IO (ClientResponse, Maybe ClientState)
handleOnlineServerMessage Nothing msg = do
  note $ "Received: " ++ show msg
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
  note $ "Received: " ++ show msg
  case msg of
    gq@GameplayQuery{..} | gqState == Nothing -> do
      Reply gr@GameplayReply{..} <- handleOfflineServerMessage (gq { gqState = jcs })
      --
      -- TODO: Do something about the game here
      --
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
