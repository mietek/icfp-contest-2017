module Main where

import Control.Concurrent
import System.Random


newtype Input = Input Int
  deriving (Eq, Ord, Show)

newtype Output = Output Int
  deriving (Eq, Ord, Show)

initialInput :: Input
initialInput = Input 0

initialOutput :: Output
initialOutput = Output 0

outputToInput :: Output -> Input
outputToInput (Output n) = Input n

isBetterThan :: Output -> Output -> Bool
isBetterThan (Output n1) (Output n2) = n1 > n2


type InputBox = MVar Input

data Msg = Result Output InputBox | Timeout

type MsgBox = MVar Msg


main :: IO ()
main = do
  mbox <- newEmptyMVar
  ibox1 <- newMVar initialInput
  ibox2 <- newMVar initialInput
  _ <- forkIO (timer mbox 1000000)
  _ <- forkIO (simulator mbox ibox1)
  _ <- forkIO (simulator mbox ibox2)
  putStrLn $ "Initial input: " ++ show initialInput ++ "\n"
  finO <- supervisor mbox initialInput initialOutput
  putStrLn $ "\nFinal output: " ++ show finO


supervisor :: MsgBox -> Input -> Output -> IO Output
supervisor mbox initI initO = loop initI initO
  where
    loop :: Input -> Output -> IO Output
    loop i bestO = do
      msg <- takeMVar mbox
      case msg of
        Result newBestO ibox | newBestO `isBetterThan` bestO -> do
          putStrLn $ "New best: " ++ show newBestO
          let newI = outputToInput newBestO
          putMVar ibox newI
          loop newI newBestO
        Result o ibox -> do
          putStrLn $ "Ignored:  " ++ show o
          putMVar ibox i
          loop i bestO
        Timeout -> do
          putStrLn "Timeout"
          return bestO

simulator :: MsgBox -> InputBox -> IO ()
simulator mbox ibox = loop
  where
    loop :: IO ()
    loop = do
      Input n <- takeMVar ibox
      threadDelay 100000
      r <- randomRIO (0, 10)
      putMVar mbox (Result (Output (n + r)) ibox)
      loop

timer :: MsgBox -> Int -> IO ()
timer mbox t = do
  threadDelay t
  putMVar mbox Timeout
