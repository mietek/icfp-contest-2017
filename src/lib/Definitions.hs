module Definitions where

import qualified Data.Aeson as JSON
import Data.Aeson (FromJSON, ToJSON, parseJSON, toJSON, (.:), (.=))
import Data.Foldable (asum)
  
  
type PunterId = Int

type SiteId = Int


data Site = Site
  { sId :: SiteId
  }
  deriving (Eq, Ord, Show)

instance FromJSON Site where
  parseJSON =
    JSON.withObject "site" $ \o -> do
      sId <- o .: "id"
      return Site{..}


data River = River
  { rSource :: SiteId
  , rTarget :: SiteId
  }
  deriving (Eq, Ord, Show)

instance FromJSON River where
  parseJSON =
    JSON.withObject "river" $ \o -> do
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
    JSON.withObject "move" $ \o -> asum
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
    JSON.object
      [ "claim" .=
          JSON.object
            [ "punter" .= cPunter
            , "source" .= cSource
            , "target" .= cTarget
            ]
      ]
  toJSON Pass{..} =
    JSON.object
      [ "pass" .=
          JSON.object
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
    JSON.withObject "score" $ \o -> do
      sPunter <- o .: "punter"
      sScore  <- o .: "score"
      return Score{..}
