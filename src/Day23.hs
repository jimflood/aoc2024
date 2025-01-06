
module Day23
    ( day23
    ) where

import Lib (gridFind, parseGrid, slurpLines, Coordinate, Grid)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe
import Data.List (intercalate, nub)
import Data.List.Split (splitOn)
import Data.Bits (xor)
import Data.List (group, groupBy, partition, sort, sortBy, subsequences)
import Data.Ord (comparing)
import Data.Function (on)

import Debug.Trace

type Graph = Map.Map String (Set.Set String)

parse :: [String] -> [[String]]
parse = map (splitOn "-")

graph :: [[String]] -> Graph
graph ps = foldl graph' Map.empty (ps ++ (map reverse ps))
    where
        graph' a (s : t : []) = Map.alter graph'' s a
            where
                graph'' Nothing = Just (Set.singleton t)
                graph'' (Just ts) = Just (Set.insert t ts)

pairsOf :: [String] -> [[String]]
pairsOf xs = [[a, b] | a <- xs, b <- xs, a < b]

triplesOf :: [String] -> [[String]]
triplesOf xs = [[a, b, c] | a <- xs, b <- xs, c <- xs, a < b, b < c]

candidates :: [[String]] -> [[String]]
candidates ps = filter hasChief $ triplesOf $ (nub . concat) ps

isThree :: Graph -> [String] -> Bool
isThree g t = all ( \ (a : b : []) -> Set.member a (g Map.! b)) (pairsOf t)

hasChief :: [String] -> Bool
hasChief = any ( \ a -> 't' == head a)

partyOf :: Graph -> Set.Set String -> [[String]]
partyOf g s = foldl partyOf' [] (subsequences (Set.toList s))
    where
        partyOf' aa bb
            | all id [Set.member a (g Map.! b) | a <- bb, b <- bb, a < b] = bb : aa
            | otherwise = aa

solve1 :: Graph -> [[String]] -> Int
solve1 g ps = length $ filter (isThree g) (candidates ps)

solve2 :: Graph -> String
solve2 g = intercalate "," $ sort $ head $ nub $ filter ((==13) . length) $ concatMap ((partyOf g) . (uncurry Set.insert)) $ Map.toList g

day23 :: IO ()
day23 = do
    xs <- slurpLines "day23.txt"
    let ps = parse xs
    let g = graph ps
    let answer1 = solve1 g ps
    print $ "part 1: " ++ (show answer1)
    let answer2 = solve2 g
    print $ "part 2: " ++ answer2
