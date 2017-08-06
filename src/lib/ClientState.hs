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

siteSetToList :: SiteSet -> [SiteId]
siteSetToList = IS.toList . unSiteSet

data Site = Site
    { sNeighbours :: SiteSet
    , sIsMine     :: Bool
    }
  deriving (Eq, Ord, Show)

instance FromJSON Site where
  parseJSON =
    JSON.withObject "site" $ \o -> do
      sNeighbours <- o .: "neighbours"
      sIsMine     <- o .: "isMine"
      return Site{..}

instance ToJSON Site where
  toJSON Site{..} =
    JSON.object
      [ "neighbours" .= sNeighbours
      , "isMine"     .= sIsMine
      ]

instance Monoid Site where
  mempty  = emptySite
  mappend = mergeSites

emptySite :: Site
emptySite = Site
  { sNeighbours = emptySiteSet
  , sIsMine     = False
  }

mergeSites :: Site -> Site -> Site
mergeSites s1 s2 = Site
  { sNeighbours = sNeighbours s1 <> sNeighbours s2
  , sIsMine     = sIsMine s1 || sIsMine s2
  }
  
neighbourSite :: SiteId -> Site
neighbourSite siteId = emptySite
  { sNeighbours = singletonSiteSet siteId
  }

mineSite :: Site
mineSite = emptySite
  { sIsMine = True
  }


newtype SiteMap = SiteMap { unSiteMap :: IntMap Site } -- Key: SiteId
  deriving (Eq, Ord, Show, FromJSON, ToJSON)

instance Monoid SiteMap where
  mempty  = emptySiteMap
  mappend = mergeSiteMaps
  
emptySiteMap :: SiteMap
emptySiteMap = SiteMap IM.empty

mergeSiteMaps :: SiteMap -> SiteMap -> SiteMap
mergeSiteMaps sm1 sm2 = SiteMap $
  IM.unionWith (<>) (unSiteMap sm1) (unSiteMap sm2)

partialSiteMapFromSiteIds :: [SiteId] -> SiteMap
partialSiteMapFromSiteIds siteIds = SiteMap $
  IM.fromList [(siteId, emptySite) | siteId <- siteIds]
  
partialSiteMapFromRivers :: [River] -> SiteMap
partialSiteMapFromRivers rivers = SiteMap $
  IM.fromList [(rSource, neighbourSite rTarget) | River{..} <- rivers]

partialSiteMapFromReverseRivers :: [River] -> SiteMap
partialSiteMapFromReverseRivers rivers = SiteMap $
  IM.fromList [(rTarget, neighbourSite rSource) | River{..} <- rivers]

partialSiteMapFromMineIds :: [SiteId] -> SiteMap
partialSiteMapFromMineIds mineIds = SiteMap $
  IM.fromList [(siteId, mineSite) | siteId <- mineIds]
  
fullSiteMap :: [SiteId] -> [River] -> [SiteId] -> SiteMap
fullSiteMap siteIds rivers mineIds =
     partialSiteMapFromSiteIds siteIds
  <> partialSiteMapFromRivers rivers
  <> partialSiteMapFromReverseRivers rivers
  <> partialSiteMapFromMineIds mineIds

lookupSite :: SiteId -> SiteMap -> Maybe Site
lookupSite siteId = IM.lookup siteId . unSiteMap


type RiverId = Int

fromRiver :: River -> RiverId
fromRiver River{..} = rSource * 1000000000 + rTarget

fromRiverId :: RiverId -> River
fromRiverId riverId =
  let (rSource, rTarget) = quotRem riverId 1000000000 in
  River{..}

fromSites :: SiteId -> SiteId -> RiverId
fromSites a b =
  fromRiver River{rSource = a, rTarget = b}


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
  , csMines       :: SiteSet
  }
  deriving (Eq, Ord, Show)

instance FromJSON ClientState where
  parseJSON =
    JSON.withObject "clientState" $ \o -> do
      csPunterId    <- o .: "punterId"
      csPunterCount <- o .: "punterCount"
      csSiteMap     <- o .: "siteMap"
      csClaimMap    <- o .: "claimMap"
      csMines       <- o .: "mines"
      return ClientState{..}

instance ToJSON ClientState where
  toJSON ClientState{..} =
    JSON.object
      [ "punterId"    .= csPunterId
      , "punterCount" .= csPunterCount
      , "siteMap"     .= csSiteMap
      , "claimMap"    .= csClaimMap
      , "mines"       .= csMines
      ]

emptyClientState :: ClientState
emptyClientState = ClientState
  { csPunterId    = 0
  , csPunterCount = 0
  , csSiteMap     = emptySiteMap
  , csClaimMap    = emptyClaimMap
  , csMines       = emptySiteSet
  }
