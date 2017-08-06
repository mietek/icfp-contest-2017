{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module ClientState where

import qualified Data.Aeson as JSON
import Data.Aeson (FromJSON, ToJSON, parseJSON, toJSON, (.:), (.=))
import qualified Data.IntMap.Strict as IM
import Data.IntMap.Strict (IntMap)
import qualified Data.IntSet as IS
import Data.List (foldl')
import Data.IntSet (IntSet)
import Data.Monoid ((<>))

import Definitions


newtype SiteSet = SiteSet { unSiteSet :: IntSet } -- Key: SiteId
  deriving (Eq, Ord, FromJSON, ToJSON)

instance Show SiteSet where
  show ss = show (IS.toAscList (unSiteSet ss))

instance Monoid SiteSet where
  mempty  = emptySiteSet
  mappend = mergeSiteSets

emptySiteSet :: SiteSet
emptySiteSet =
  SiteSet IS.empty

mergeSiteSets :: SiteSet -> SiteSet -> SiteSet
mergeSiteSets ss1 ss2 = SiteSet $
  IS.union (unSiteSet ss1) (unSiteSet ss2)

singletonSiteSet :: SiteId -> SiteSet
singletonSiteSet sid = SiteSet $
  IS.singleton sid

memberSite :: SiteSet -> SiteId -> Bool
memberSite ss sid =
  IS.member sid (unSiteSet ss)

siteSetToList :: SiteSet -> [SiteId]
siteSetToList = IS.toList . unSiteSet

data SiteInfo = SiteInfo
    { siNeighbours :: SiteSet
    , siIsMine     :: Bool
    }
  deriving (Eq, Ord)

instance Show SiteInfo where
  show SiteInfo{..} = "neighbours: " ++ show siNeighbours ++ "; " ++
                      "mine: " ++ if siIsMine then "yes" else "no"

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
neighbourSite sid = emptySite
  { siNeighbours = singletonSiteSet sid
  }

mineSite :: SiteInfo
mineSite = emptySite
  { siIsMine = True
  }


newtype SiteMap = SiteMap { unSiteMap :: IntMap SiteInfo } -- Key: SiteId
  deriving (Eq, Ord, FromJSON, ToJSON)

instance Show SiteMap where
  show sm = show (IM.toAscList (unSiteMap sm))

instance Monoid SiteMap where
  mempty  = emptySiteMap
  mappend = mergeSiteMaps

emptySiteMap :: SiteMap
emptySiteMap =
  SiteMap IM.empty

mergeSiteMaps :: SiteMap -> SiteMap -> SiteMap
mergeSiteMaps sm1 sm2 = SiteMap $
  IM.unionWith (<>) (unSiteMap sm1) (unSiteMap sm2)

partialSiteMapFromSites :: [Site] -> SiteMap
partialSiteMapFromSites sites = SiteMap $
  IM.fromList [(sId, emptySite) | Site{..} <- sites]

partialSiteMapFromRivers :: [River] -> SiteMap
partialSiteMapFromRivers rivers = SiteMap $
  IM.fromListWith (<>) [(rSource, neighbourSite rTarget) | River{..} <- rivers]

partialSiteMapFromRevRivers :: [River] -> SiteMap
partialSiteMapFromRevRivers rivers = SiteMap $
  IM.fromListWith (<>) [(rTarget, neighbourSite rSource) | River{..} <- rivers]

partialSiteMapFromMineIds :: [SiteId] -> SiteMap
partialSiteMapFromMineIds mineIds = SiteMap $
  IM.fromList [(sid, mineSite) | sid <- mineIds]

fullSiteMap :: [Site] -> [River] -> [SiteId] -> SiteMap
fullSiteMap sites rivers mineIds =
     partialSiteMapFromSites sites
  <> partialSiteMapFromRivers rivers
  <> partialSiteMapFromRevRivers rivers
  <> partialSiteMapFromMineIds mineIds

lookupSite :: SiteMap -> SiteId -> Maybe SiteInfo
lookupSite sm sid =
  IM.lookup sid (unSiteMap sm)

siteCount :: SiteMap -> Int
siteCount sm =
  IM.size (unSiteMap sm)


type RiverId = Int

lots :: Int
lots = 1000000000

riverId :: Int -> Int -> RiverId
riverId source target =
  source * lots + target

fromRiver :: River -> RiverId
fromRiver River{..} =
  riverId rSource rTarget

toRiver :: RiverId -> River
toRiver rid =
  let (rSource, rTarget) = quotRem rid lots in
  River{..}

fromSites :: SiteId -> SiteId -> RiverId
fromSites a b =
  fromRiver River{rSource = a, rTarget = b}

newtype ClaimMap = ClaimMap { unClaimMap :: IntMap PunterId } -- Key: RiverId
  deriving (Eq, Ord, FromJSON, ToJSON)

instance Show ClaimMap where
  show cm = show (map toClaim (IM.toAscList (unClaimMap cm)))
    where
      toClaim (rid, pid) =
        let River{..} = toRiver rid in
        Claim
          { cPunter = pid
          , cSource = rSource
          , cTarget = rTarget
          }

emptyClaimMap :: ClaimMap
emptyClaimMap =
  ClaimMap IM.empty

insertClaim :: ClaimMap -> RiverId -> PunterId -> ClaimMap
insertClaim cm rid pid =
  if IM.member rid (unClaimMap cm)
    then error $ "duplicate riverId: " ++ show rid
    else ClaimMap $
      IM.insert rid pid (unClaimMap cm)

insertMove :: ClaimMap -> Move -> ClaimMap
insertMove cm Pass{..}  = cm
insertMove cm Claim{..} = insertClaim cm (riverId cSource cTarget) cPunter

insertMoves :: ClaimMap -> [Move] -> ClaimMap
insertMoves cm moves =
  foldl' insertMove cm moves

lookupClaim :: ClaimMap -> RiverId -> Maybe PunterId
lookupClaim cm rid =
  IM.lookup rid (unClaimMap cm)

claimCount :: ClaimMap -> Int
claimCount cm =
  IM.size (unClaimMap cm)


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
      let csMines   = SiteSet . IM.keysSet $ IM.filter (siIsMine) (unSiteMap csSiteMap)
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
