module Game where

import Definitions
import ClientState
import Control.Applicative ((<|>))
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.List (find)

type Scores = [Score]

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
traverseD _ [] _ _ = 0
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

freeRivers :: ClientState -> [River]
freeRivers ClientState{..} =
    IM.foldrWithKey reduce [] (unSiteMap csSiteMap)
  where
    reduce site SiteInfo{..} acc = (freeConnections site siNeighbours) ++ acc
    freeConnections s ns = filter isFree $ orderedRivers s ns
    isFree r = case (lookupClaim csClaimMap $ fromRiver r) of
      Just (-1) -> True
      Nothing -> True
      _ -> False
    orderedRivers s ns = filter (\r -> rSource r < rTarget r) $ rivers s ns
    rivers s ns = map (\x -> River s x) $ siteSetToList ns


scores :: ClientState -> Scores
scores ClientState{..} =
    map (\p -> Score { sPunter = p, sScore = scoreForPunter p }) $ take csPunterCount [0..]
  where
    scoreForPunter :: PunterId -> Int
    scoreForPunter p = sum $ map (scoreForPunterAtMine p) (siteSetToList csMines)
    scoreForPunterAtMine :: PunterId -> SiteId -> Int
    scoreForPunterAtMine p m = traverseAndCollectPoints p m [m] 0 []
    traverseAndCollectPoints :: PunterId -> SiteId -> [SiteId] -> Int -> [SiteId] -> Int
    traverseAndCollectPoints _ _ [] score _ = score
    traverseAndCollectPoints p m (s:ss) score visited =
        traverseAndCollectPoints p m (nextSites ++ ss) (siteScore+score) (s:visited)
      where
        siteScore = d*d
        d = distance csSiteMap m s
        nextSites = filter (\x -> not . elem x $ (ss++s:visited)) $ reachableNeighbours
        reachableNeighbours = filter (reachable csClaimMap p s) $ neighboursList csSiteMap s

isMyRiver :: ClientState -> RiverId -> Bool
isMyRiver ClientState{..} rId =
  case (lookupClaim csClaimMap rId) of
    Just pId -> pId == csPunterId
    _        -> False

isMyMine :: ClientState -> SiteId -> Bool
isMyMine cs@ClientState{..} sId =
  let
    mine = lookupSite csSiteMap sId
    neighbours = case mine of
      Just SiteInfo{..} -> unSiteSet siNeighbours
      Nothing -> IS.empty
    mineRivers = IS.union
      (IS.map (\nId -> riverId sId nId) neighbours)
      (IS.map (\nId -> riverId nId sId) neighbours)
    myRivers = IS.filter (isMyRiver cs) mineRivers
  in
    IS.size myRivers > 0

minesScore :: ClientState -> Float
minesScore cs@ClientState{..} =
  let
    allMines = unSiteSet csMines
    myMines = IS.filter (isMyMine cs) allMines
    myMinesCount = IS.size myMines
    allMinesCount = IS.size allMines
  in
    (fromIntegral myMinesCount) / (fromIntegral allMinesCount)

value :: ClientState -> Float
value cs@ClientState{..} =
  let
    myScore = case find (\Score{..} -> sPunter == csPunterId) (scores cs) of
      Just Score{..} -> sScore
      Nothing -> 0
    allScores = foldr (\Score{..} t -> t + sScore) 0 $ scores cs
    scoreComponent = (fromIntegral myScore) / (fromIntegral allScores)
    minesComponent = minesScore cs
  in
    0.95 * scoreComponent + 0.05 * minesComponent
