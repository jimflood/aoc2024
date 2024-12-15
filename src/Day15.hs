
module Day15
    ( day15
    ) where

import Lib (slurpLines, opchunk, Coordinate, Grid, parseGrid)
import Data.List ((\\), nub)
import qualified Data.Map as Map

type Move = Char

parse :: [String] -> (Grid, [Move])
parse = (parse' . opchunk)
    where
        parse' [xs, ys] = (((Map.filter (/='.')) . snd . parseGrid) xs, concat ys)
        parse' _ = error "bad input"

next :: Move -> Coordinate -> Coordinate
next m (x, y) = next' m
    where
        next' '>' = (x + 1, y)
        next' '<' = (x - 1, y)
        next' 'v' = (x, y + 1)
        next' '^' = (x, y - 1)
        next' _ = error "nope"

body :: Grid -> Coordinate -> [Coordinate]
body g p@(x, y)
    | Map.notMember p g = []
    | g Map.! p == '[' = [(x, y), (x + 1, y)]
    | g Map.! p == ']' = [(x - 1, y), (x, y)]
    | g Map.! p == 'O' = [p]
    | otherwise = []

clump :: Grid -> Move -> [Coordinate]
clump g m = clump' [] [robot]
    where
        robot = fst $ head $ filter ( \ (_, v) -> v == '@') (Map.toList g)
        clump' acc [] = acc
        clump' acc ps = clump' (nub (ps ++ acc)) (concatMap (body g) (map (next m) (ps \\ acc)))

changes :: Grid -> Move -> [Coordinate] -> [Coordinate]
changes g m ps = changes' $ map (next m) ps
    where
        changes' nps
            | all ( \ np -> Map.notMember np g) (nps \\ ps) = nps
            | otherwise = []

push :: Grid -> Move -> Grid
push g m = push' (clump g m)
    where
        push' ps = push'' (changes g m ps)
            where
                push'' [] = g
                push'' nps = update $ Map.fromList (zip ps nps)
                update a = Map.mapKeys ( \ x -> Map.findWithDefault x x a) g

run :: Grid -> [Move] -> Grid
run g ms = foldl ( \ a b -> push a b) g ms

stretch :: Grid -> Grid
stretch g = stretch' [] $ Map.toList g
    where
        stretch' acc [] = Map.fromList acc
        stretch' acc (((x, y), c) : ps)
            | c == 'O' = stretch' (((x * 2, y), '[') : ((x * 2 + 1, y), ']') : acc) ps
            | c == '@' = stretch' (((x * 2, y), '@') : acc) ps
            | otherwise = stretch' (((x * 2, y), c) : ((x * 2 + 1, y), c) : acc) ps

gps :: Coordinate -> Int
gps (x, y) = (100 * y) + x

solve :: Grid -> [Move] -> Int
solve g ms = sum $ map gps $ map fst $ filter ( \ (_, v) -> v == 'O' || v == '[') (Map.toList (run g ms))

day15 :: IO ()
day15 = do
    xs <- slurpLines "day15.txt"
    let (g, ms) = parse xs
    let answer1 = solve g ms
    print $ "part 1: " ++ (show answer1)
    let answer2 = solve (stretch g) ms
    print $ "part 2: " ++ (show answer2)
