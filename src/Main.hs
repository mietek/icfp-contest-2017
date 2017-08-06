module Main where

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import System.Exit (exitSuccess)
import System.IO (Handle, hPutStrLn, stderr, stdin, stdout)

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
  | NoReply
  | Exit
  deriving (Eq, Ord, Show)

handleOfflineServerMessage :: ServerMessage -> IO ClientResponse
handleOfflineServerMessage msg =
  case msg of
    hr@HandshakeReply{..} -> do
      note $ "Handshake reply received: " ++ show hr
      return NoReply
    sq@SetupQuery{..} -> do
      note $ "Setup query received: " ++ show sq
      return $ Reply $ SetupReply
        { srReady = sqPunter
        , srState = Just $ emptyClientState
          { csPunterId    = sqPunter
          , csPunterCount = sqPunters
          , csSiteMap     = fullSiteMap sqSites sqRivers sqMines
          }
        }
    gq@GameplayQuery{..} -> do
      note $ "Gameplay query received: " ++ show gq
      cs@ClientState{..} <- assertJust gqState "missing state in gameplay query"
      return $ Reply $ GameplayReply
        { grMove  = Pass { pPunter = csPunterId }
        , grState = Just cs
        }
    sn@ScoringNotice{..} -> do
      note $ "Scoring notice received: " ++ show sn
      cs@ClientState{..} <- assertJust snState "missing state in scoring notice"
      return Exit
    tn@TimeoutNotice{..} -> do
      note $ "Timeout notice received: " ++ show tn
      return NoReply

handleOnlineServerMessage :: Maybe ClientState -> ServerMessage -> IO (ClientResponse, Maybe ClientState)
handleOnlineServerMessage Nothing msg =
  case msg of
    hr@HandshakeReply{..} -> do
      NoReply <- handleOfflineServerMessage hr
      return (NoReply, Nothing)
    sq@SetupQuery{..} -> do
      Reply sr@SetupReply{..} <- handleOfflineServerMessage sq
      return (Reply sr { srState = Nothing }, srState)
    _ ->
      error $ "unexpected server message in stateless phase: " ++ show msg
handleOnlineServerMessage jcs@(Just cs@ClientState{..}) msg =
  case msg of
    gq@GameplayQuery{..} | gqState == Nothing -> do
      Reply gr@GameplayReply{..} <- handleOfflineServerMessage (gq { gqState = jcs })
      return (Reply gr { grState = Nothing }, grState)
    sn@ScoringNotice{..} | snState == Nothing -> do
      Exit <- handleOfflineServerMessage (sn { snState = jcs })
      return (Exit, jcs)
    tn@TimeoutNotice{..} -> do
      NoReply <- handleOfflineServerMessage tn
      return (NoReply, jcs)
    _ ->
      error $ "unexpected server message in stateful phase: " ++ show msg


onlineMain :: String -> Int -> String -> IO ()
onlineMain serverHost serverPort punterName = do
  note $ "Online mode (" ++ serverHost ++ ":" ++ show serverPort ++ ")"
  -- TODO


offlineMain :: String -> IO ()
offlineMain punterName = do
  note "Offline mode"
  putClientMessage stdout (HandshakeQuery { hqMe = punterName })
  msg <- getServerMessage stdin
  response <- handleOfflineServerMessage msg
  case response of
    Reply reply -> do
      note $ "Replying: " ++ show reply
      putClientMessage stdout reply
    NoReply ->
      note "No reply"
    Exit ->
      return ()
  note "Exiting..."


main :: IO ()
main = do
  Options{..} <- getOptions
  note oPunterName
  if oOnlineMode
    then onlineMain oServerHost oServerPort oPunterName
    else offlineMain oPunterName
