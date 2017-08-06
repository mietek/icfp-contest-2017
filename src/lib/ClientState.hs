{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module ClientState where

import qualified Data.Aeson as JSON
import Data.Aeson (FromJSON, ToJSON, parseJSON, toJSON, (.:), (.=))
import qualified Data.IntMap.Strict as IM
import Data.IntMap.Strict (IntMap)
import qualified Data.IntSet as IS
import Data.IntSet (IntSet)
import Data.Monoid ((<>))

import Definitions


newtype SiteSet = SiteSet { unSiteSet :: IntSet } -- Key: SiteId
  deriving (Eq, Ord, Show, FromJSON, ToJSON)

instance Monoid SiteSet where
  mempty  = emptySiteSet
  mappend = mergeSiteSets
  
emptySiteSet :: SiteSet
emptySiteSet = SiteSet IS.empty

mergeSiteSets :: SiteSet -> SiteSet -> SiteSet
mergeSiteSets ss1 ss2 = SiteSet $
  IS.union (unSiteSet ss1) (unSiteSet ss2)
  
singletonSiteSet :: SiteId -> SiteSet
singletonSiteSet siteId = SiteSet $
  IS.singleton siteId
  
memberSite :: SiteId -> SiteSet -> Bool
memberSite siteId ss =
  IS.member siteId (unSiteSet ss)


data SiteInfo = SiteInfo
    { siNeighbours :: SiteSet
    , siIsMine     :: Bool
    }
  deriving (Eq, Ord, Show)

instance FromJSON SiteInfo where
  parseJSON =
    JSON.withObject "SiteInfo" $ \o -> do
      siNeighbours <- o .: "neighbours"
      siIsMine     <- o .: "isMine"
      return SiteInfo{..}
  
instance ToJSON SiteInfo where
  toJSON SiteInfo{..} =
    JSON.object
      [ "neighbours" .= siNeighbours
      , "isMine"     .= siIsMine
      ]

instance Monoid SiteInfo where
  mempty  = emptySite
  mappend = mergeSites

emptySite :: SiteInfo
emptySite = SiteInfo
  { siNeighbours = emptySiteSet
  , siIsMine     = False
  }

mergeSites :: SiteInfo -> SiteInfo -> SiteInfo
mergeSites s1 s2 = SiteInfo
  { siNeighbours = siNeighbours s1 <> siNeighbours s2
  , siIsMine     = siIsMine s1 || siIsMine s2
  }
  
neighbourSite :: SiteId -> SiteInfo
neighbourSite siteId = emptySite
  { siNeighbours = singletonSiteSet siteId
  }

mineSite :: SiteInfo
mineSite = emptySite
  { siIsMine = True
  }


newtype SiteMap = SiteMap { unSiteMap :: IntMap SiteInfo } -- Key: SiteId
  deriving (Eq, Ord, Show, FromJSON, ToJSON)

instance Monoid SiteMap where
  mempty  = emptySiteMap
  mappend = mergeSiteMaps
  
emptySiteMap :: SiteMap
emptySiteMap = SiteMap IM.empty

mergeSiteMaps :: SiteMap -> SiteMap -> SiteMap
mergeSiteMaps sm1 sm2 = SiteMap $
  IM.unionWith (<>) (unSiteMap sm1) (unSiteMap sm2)

partialSiteMapFromSites :: [Site] -> SiteMap
partialSiteMapFromSites sites = SiteMap $
  IM.fromList [(sId, emptySite) | Site{..} <- sites]
  
partialSiteMapFromRivers :: [River] -> SiteMap
partialSiteMapFromRivers rivers = SiteMap $
  IM.fromList [(rSource, neighbourSite rTarget) | River{..} <- rivers]

partialSiteMapFromReverseRivers :: [River] -> SiteMap
partialSiteMapFromReverseRivers rivers = SiteMap $
  IM.fromList [(rTarget, neighbourSite rSource) | River{..} <- rivers]

partialSiteMapFromMineIds :: [SiteId] -> SiteMap
partialSiteMapFromMineIds mineIds = SiteMap $
  IM.fromList [(siteId, mineSite) | siteId <- mineIds]
  
fullSiteMap :: [Site] -> [River] -> [SiteId] -> SiteMap
fullSiteMap sites rivers mineIds =
     partialSiteMapFromSites sites
  <> partialSiteMapFromRivers rivers
  <> partialSiteMapFromReverseRivers rivers
  <> partialSiteMapFromMineIds mineIds

lookupSite :: SiteId -> SiteMap -> Maybe SiteInfo
lookupSite siteId = IM.lookup siteId . unSiteMap


type RiverId = Int

fromRiver :: River -> RiverId
fromRiver River{..} = rSource * 1000000000 + rTarget

fromRiverId :: RiverId -> River
fromRiverId riverId =
  let (rSource, rTarget) = quotRem riverId 1000000000 in
  River{..}


newtype ClaimMap = ClaimMap { unClaimMap :: IntMap PunterId } -- Key: RiverId
  deriving (Eq, Ord, Show, FromJSON, ToJSON)
  
emptyClaimMap :: ClaimMap
emptyClaimMap = ClaimMap IM.empty

insertClaim :: RiverId -> PunterId -> ClaimMap -> ClaimMap
insertClaim riverId punterId cm =
  if IM.member riverId (unClaimMap cm)
    then error $ "duplicate riverId: " ++ show riverId
    else ClaimMap $
      IM.insert riverId punterId (unClaimMap cm)

lookupClaim :: RiverId -> ClaimMap -> Maybe PunterId
lookupClaim riverId = IM.lookup riverId . unClaimMap

        
data ClientState = ClientState
  { csPunterId    :: Int
  , csPunterCount :: Int
  , csSiteMap     :: SiteMap
  , csClaimMap    :: ClaimMap
  }
  deriving (Eq, Ord, Show)
  
instance FromJSON ClientState where
  parseJSON =
    JSON.withObject "clientState" $ \o -> do
      csPunterId    <- o .: "punterId"
      csPunterCount <- o .: "punterCount"
      csSiteMap     <- o .: "siteMap"
      csClaimMap    <- o .: "claimMap"
      return ClientState{..}
  
instance ToJSON ClientState where
  toJSON ClientState{..} =
    JSON.object
      [ "punterId"    .= csPunterId
      , "punterCount" .= csPunterCount
      , "siteMap"     .= csSiteMap
      , "claimMap"    .= csClaimMap
      ]

emptyClientState :: ClientState
emptyClientState = ClientState
  { csPunterId    = 0
  , csPunterCount = 0
  , csSiteMap     = emptySiteMap
  , csClaimMap    = emptyClaimMap
  }
