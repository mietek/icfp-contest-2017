module RulesTest where

import Test.Tasty
import Test.Tasty.HUnit

import ClientState
import Definitions
import Game
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.Monoid ((<>))

trivialClientState :: ClientState
trivialClientState = ClientState
  { csPunterId    = 0
  , csPunterCount = 1
  , csSiteMap     = emptySiteMap
  , csClaimMap    = emptyClaimMap
  , csMines       = emptySiteSet
  }

simpleSiteMap :: SiteMap
simpleSiteMap =
  fullSiteMap
    (map Site [0,1,2,3,4])
    [ River{rSource = 0, rTarget = 1}
    , River{rSource = 2, rTarget = 3}
    , River{rSource = 3, rTarget = 4}
    , River{rSource = 1, rTarget = 3}
    , River{rSource = 1, rTarget = 2}
    ]
    [1]

simpleClaimMap :: ClaimMap
simpleClaimMap =
    foldr (\c m -> insertClaim m c 0) emptyClaimMap claimedRivers
  where
    claimedRivers = map fromRiver
      [ River{rSource = 1, rTarget = 2}
      , River{rSource = 2, rTarget = 3}
      , River{rSource = 3, rTarget = 4}
      ]

simpleClientState :: ClientState
simpleClientState = ClientState
    { csPunterId    = 0
    , csPunterCount = 1
    , csSiteMap     = simpleSiteMap
    , csClaimMap    = simpleClaimMap
    , csMines       = singletonSiteSet 1
    }

unit_neighboursList =
  neighboursList simpleSiteMap 1 @?= [0,2,3]

unit_siteMapSimple =
  simpleSiteMap @?= SiteMap
    { unSiteMap = IM.fromList
      [ (0, SiteInfo {siNeighbours = SiteSet {unSiteSet = IS.fromList [1]}, siIsMine = False})
      , (1, SiteInfo {siNeighbours = SiteSet {unSiteSet = IS.fromList [0,2,3]}, siIsMine = True})
      , (2, SiteInfo {siNeighbours = SiteSet {unSiteSet = IS.fromList [1,3]}, siIsMine = False})
      , (3, SiteInfo {siNeighbours = SiteSet {unSiteSet = IS.fromList [1,2,4]}, siIsMine = False})
      , (4, SiteInfo {siNeighbours = SiteSet {unSiteSet = IS.fromList [3]}, siIsMine = False})
      ]
    }

unit_scoreEmpty =
  scores emptyClientState @?= []

unit_scoreTrivial =
  scores trivialClientState @?= [
    Score
      { sPunter = 0
      , sScore  = 0
      }
    ]
unit_scoreSimple =
  scores simpleClientState @?= [
    Score
      { sPunter = 0
      , sScore  = 4
      }
    ]

unit_distance =
  distance simpleSiteMap 0 4 @?= 3

unit_reachable =
  reachable simpleClaimMap 0 1 2 @?= True

unit_reachableReverse =
  reachable simpleClaimMap 0 2 1 @?= True

unit_nonReachable =
  reachable simpleClaimMap 0 0 4 @?= False
