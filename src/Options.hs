module Options
  ( Options(..)
  , getOptions
  ) where

import Data.Monoid ((<>))
import Options.Applicative (Parser)
import qualified Options.Applicative as P


data Options = Options
  { oOnlineMode :: Bool
  , oServerHost :: String
  , oServerPort :: Int
  , oPunterName :: String
  }
  deriving (Eq, Show)

getOptions :: IO Options
getOptions =
  P.execParser $
    P.info (P.helper <*> options) $
         P.header "punter"
      <> P.progDesc "A lambda punter client"
      <> P.fullDesc


options :: Parser Options
options =
  Options <$>
        onlineMode
    <*> serverHost
    <*> serverPort
    <*> punterName

onlineMode :: Parser Bool
onlineMode =
  P.switch $
       P.long "online-mode"
    <> P.short 'o'
    <> P.help "Connect to a lambda punter server"
    
serverHost :: Parser String
serverHost =
  P.option P.auto $
       P.long "server-host"
    <> P.short 'h'
    <> P.metavar "SERVER_HOST"
    <> P.value "punter.inf.ed.ac.uk"
    <> P.showDefault
    <> P.help "Server host"
      
serverPort :: Parser Int
serverPort =
  P.option P.auto $
       P.long "server-port"
    <> P.short 'p'
    <> P.metavar "SERVER_PORT"
    <> P.value 9000
    <> P.showDefault
    <> P.help "Server port"

punterName :: Parser String
punterName =
  P.option P.auto $
       P.long "punter-name"
    <> P.short 'n'
    <> P.metavar "PUNTER_NAME"
    <> P.value "The Church of the Least Fixed Punt"
    <> P.showDefault
    <> P.help "Name of the lambda punter client"
