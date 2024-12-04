
module Day4
    ( day4
    ) where

import Lib (slurpLines, Coordinate, Grid, parseGrid)
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Maybe

rose8 :: [(Int, Int)]
rose8 = [(-1, -1), (0, -1), (1, -1), (-1, 0), (1, 0), (-1, 1), (0, 1), (1, 1)]

rose4 :: [(Int, Int)]
rose4 = [(-1, -1), (1, -1), (-1, 1), (1, 1)]

leys :: [(Int, Int)] -> Int -> Coordinate -> [[Coordinate]]
leys r len (x, y) = map leys' r
    where
        leys' (dx, dy) = map ( \ n -> (x + (n * dx), y + (n * dy))) (take len [0..])

matches :: Grid -> [(Int, Int)] -> String -> [[Coordinate]]
matches m r s = concat $ Map.mapWithKey ( \ k _ -> matchesOf k) m
    where
        matchesOf c = mapMaybe matchesOf' (leys r (length s) c)
        matchesOf' ks
            | mapMaybe ( \ k -> Map.lookup k m) ks == s = Just ks
            | otherwise = Nothing

solve :: Grid -> Int
solve m = length $ matches m rose8 "XMAS"

solve2 :: Grid -> Int
solve2 m = length $ solve2' $ matches m rose4 "MAS"
    where
        solve2' ccs = filter (==2) $ map length $ List.group $ List.sort $ map ( \ [_, c, _] -> c) ccs

day4 :: IO ()
day4 = do
    xs <- slurpLines "day4.txt"
    let (_, m) = parseGrid xs
    let answer1 = solve m
    print $ "part 1: " ++ (show answer1)
    let answer2 = solve2 m
    print $ "part 2: " ++ (show answer2)
