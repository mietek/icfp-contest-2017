module Main where

import qualified Data.ByteString.Char8 as S
import Data.ByteString.Char8 (ByteString)
import System.IO (Handle, hPutStrLn, stderr, stdin)

import Options
import Protocol


main :: IO ()
main = do
  Options{..} <- getOptions
  if oOnline
    then onlineMain oHost oPort
    else offlineMain


onlineMain :: String -> Int -> IO ()
onlineMain host port = do
  hPutStrLn stderr $ "Online mode (" ++ host ++ ":" ++ show port ++ ")"


offlineMain :: IO ()
offlineMain = do
  hPutStrLn stderr "Offline mode"


getMessageData :: Handle -> IO ByteString
getMessageData hdl = do
  firstPart <- S.hGetSome hdl 10
  let (lengthPart, otherPart) = S.break ((==) ':') firstPart
      totalLength = read (S.unpack lengthPart)
      remainingLength = totalLength - (S.length otherPart - 1)
  remainingPart <- S.hGetSome hdl remainingLength
  return (S.append (S.tail otherPart) remainingPart)
