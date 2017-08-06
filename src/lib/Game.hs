module Game where

import Definitions
import ClientState
import qualified Data.IntSet as IS
import Control.Applicative ((<|>))

type Scores = [Score]
--

reachable :: ClaimMap -> PunterId -> SiteId -> SiteId -> Bool
reachable cm p1 s1 s2 =
    case lookups of
      Just p2 -> p1 == p2
      Nothing -> False
  where
    lookups = lookupClaim cm (fromSites s1 s2) <|> lookupClaim cm (fromSites s2 s1)

distance :: SiteMap -> SiteId -> SiteId -> Int
distance sm from to =
  traverseD sm [(from, 0)] to []

traverseD :: SiteMap -> [(SiteId, Int)] -> SiteId -> [SiteId] -> Int
traverseD sm [] to visited = 0
traverseD sm ((s,d):ss) to visited
    | s == to = d
    | otherwise = traverseD sm (ss ++ queue) to (s:visited)
  where
    queue = map (\x -> (x, d+1)) nextSites
    nextSites = filter (\x -> not . elem x $ (s:visited)) $ neighboursList sm s

neighboursList :: SiteMap -> SiteId -> [SiteId]
neighboursList sm s = case lookupSite sm s of
  Nothing -> []
  Just SiteInfo{..} ->
    siteSetToList siNeighbours

scores :: ClientState -> Scores
scores ClientState{..} =
    map (\p -> Score { sPunter = p, sScore = scoreForPunter p }) $ take csPunterCount [0..]
  where
    scoreForPunter p = sum $ map (scoreForPunterAtMine p) (siteSetToList csMines)
    scoreForPunterAtMine p m = traverseAndCollectPoints p m [m] 0 []
    traverseAndCollectPoints p m [] score visited = score
    traverseAndCollectPoints p m (s:ss) score visited =
        traverseAndCollectPoints p m (nextSites ++ ss) (siteScore+score) (s:visited)
      where
        siteScore = (distance csSiteMap m s)^2
        nextSites = filter (\x -> not . elem x $ (ss++s:visited)) $ reachableNeighbours
        reachableNeighbours = filter (reachable csClaimMap p s) $ neighboursList csSiteMap s
