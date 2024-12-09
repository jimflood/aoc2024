
module Day8
    ( day8
    ) where

import Lib (slurpLines, Coordinate, Grid, parseGrid)
import qualified Data.Map as Map
import Data.List (groupBy, nub, subsequences, sort)
import Data.Tuple (swap)

pairs :: Grid -> [[Coordinate]]
pairs g = concat $ map pairs' $ groupBy ( \ a b -> fst a == fst b) $ sort $ map swap (Map.toList $ Map.filter (/= '.') g)
    where
        pairs' xs = filter ((2==).length) $ subsequences (map snd xs)

antinodes :: [Coordinate] -> [Coordinate]
antinodes [(a, b), (c, d)] = [(a + (a - c), b + (b - d)), (c + (c - a), d + (d - b))]

hantinodes :: [Coordinate] -> [Coordinate]
hantinodes [(a, b), (c, d)] = hantinodes' [] (-50)
    where
        hantinodes' acc n
            | n < 50 = hantinodes' ((a + (n * (a - c)), b + (n * (b - d))) : (c + (n * (c - a)), d + (n * (d - b))) : acc) (n + 1)
            | otherwise = acc

solve :: Grid -> ([Coordinate] -> [Coordinate]) -> [[Coordinate]] -> Int
solve g f xs = length $ nub $ filter ( \ k -> Map.member k g) $ concatMap f xs

day8 :: IO ()
day8 = do
    xs <- slurpLines "day8.txt"
    let (_, g) = parseGrid xs
    let answer1 = solve g antinodes (pairs g)
    print $ "part 1: " ++ (show answer1)
    let answer2 = solve g hantinodes (pairs g)
    print $ "part 2: " ++ (show answer2)
