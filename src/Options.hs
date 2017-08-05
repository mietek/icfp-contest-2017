module Options
  ( Options(..)
  , getOptions
  ) where

import Data.Monoid ((<>))
import Options.Applicative (Parser)
import qualified Options.Applicative as P


data Options = Options
  { oOnline :: Bool
  , oHost   :: String
  , oPort   :: Int
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
        online
    <*> host
    <*> port

online :: Parser Bool
online =
  P.switch $
       P.long "online"
    <> P.short 'o'
    <> P.help "Connect to a lambda punter server"
    
host :: Parser String
host =
  P.option P.auto $
       P.long "host"
    <> P.short 'h'
    <> P.metavar "HOST"
    <> P.value "punter.inf.ed.ac.uk"
    <> P.showDefault
    <> P.help "Server host"
      
port :: Parser Int
port =
  P.option P.auto $
       P.long "port"
    <> P.short 'p'
    <> P.metavar "PORT"
    <> P.value 9000
    <> P.showDefault
    <> P.help "Server port"
