
module Day6
    ( day6
    ) where

import Lib (slurpLines, Coordinate, Grid, parseGrid)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe

type Guard = (Coordinate, Char)

step :: Guard -> Grid -> Guard
step (c, d) m
    | Map.lookup (next c) m == Just '#' = (c, turn)
    | otherwise = (next c, d)
    where
        next (x, y)
            | d == '^' = (x, y - 1)
            | d == '>' = (x + 1, y)
            | d == 'v' = (x, y + 1)
            | d == '<' = (x - 1, y)
        turn
             | d == '^' = '>'
             | d == '>' = 'v'
             | d == 'v' = '<'
             | d == '<' = '^'

run :: Guard -> Grid -> (Set.Set Guard, Maybe Guard)
run = run' (Set.empty, Nothing)
    where
        run' (s, _) g m
            | Set.member g s = (s, Just g)
            | Map.member (fst g) m = run' (Set.insert g s, Nothing) (step g m) m
            | otherwise = (s, Nothing)

solve :: Guard -> Grid -> Int
solve g m = Set.size $ Set.map fst $ fst $ run g m

solve2 :: Guard -> Grid -> Int
solve2 g m = length $ filter isJust $ map solve2' $ Map.keys (Map.filter (/= '#') m)
    where
        solve2' :: Coordinate -> Maybe Guard
        solve2' c = snd (run g (Map.insert c '#' m))

day6 :: IO ()
day6 = do
    xs <- slurpLines "day6.txt"
    let (_, m) = parseGrid xs
    let g = head $ Map.toList $ Map.filter (=='^') m
    let answer1 = solve g m
    print $ "part 1: " ++ (show answer1)
    let answer2 = solve2 g m
    print $ "part 2: " ++ (show answer2)
