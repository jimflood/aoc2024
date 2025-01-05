
module Day16
    ( day16
    ) where

import Lib (slurpLines, Coordinate, Grid, gridFind, parseGrid)
import Data.List (groupBy, nub, sortBy)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Function (on)
import Data.Maybe
import Data.Ord (comparing)

type Port = Char

type Node = (Coordinate, Port)

type DirectedGraph = Map.Map Node [Node]

graph :: Grid -> DirectedGraph
graph g = Map.fromList $ map listify $ groupBy ((==) `on` fst) $ sortBy (comparing fst) $ intra ++ inter
    where
        listify xs = ((fst . head) xs, map snd xs)
        intra = [((p, a), (p, b)) | p <- (nub (map fst candidates)), a <- "^v<>", b <- "^v<>", a /= b, notElem [a, b] ["<>", "><", "^v", "v^"]]
        inter = map ( \ n -> (n, next n)) $ filter viable candidates
        candidates = concatMap nodesOf (Map.keys (Map.filter (/='#') g))
        nodesOf p = map ( \ a -> (p, a)) "^v><"
        viable n = Map.lookup ((fst . next) n) g /= Just '#'

next :: Node -> Node
next ((x, y), d)
    | d == '^' = ((x, y - 1), d)
    | d == 'v' = ((x, y + 1), d)
    | d == '<' = ((x - 1, y), d)
    | d == '>' = ((x + 1, y), d)
    | otherwise = error "ruh-roh"

solve :: Grid -> (Int, Int)
solve g = solve' (graph g)
    where
        solve' dg = solve'' (Nothing, Set.empty) (Map.fromList [fst reindeer], [reindeer])
            where
                solve'' (acc, s) (_, []) = (fromJust acc, Set.size s)
                solve'' (acc, s) (m, (((n@(p, _), c), ps) : rs))
                    | p == goal && maybe True ((<) c) acc = solve'' ((Just c), ps) (m, rs)
                    | p == goal && maybe False ((==) c) acc = solve'' (acc, Set.union s ps) (m, rs)
                    | p == goal = solve'' (acc, s) (m, rs)
                    | otherwise = solve'' (acc, s) (foldl solve''' (m, rs) (map branch (dg Map.! n)))
                    where
                        solve''' (m2, rs2) r2@((n2, c2), _)
                            | (Map.member n2 m2) && ((m2 Map.! n2) < c) = (m2, rs2)
                            | otherwise = (Map.insert n2 c2 m2, sortBy (comparing (Set.size . snd)) (r2 : rs2))
                        branch nn@(np, _)
                            | p == np = ((nn, c + 1000), Set.insert np ps)
                            | otherwise = ((nn, c + 1), Set.insert np ps)
        reindeer = (((start, '>'), 0), Set.singleton start)
        start = gridFind 'S' g
        goal = gridFind 'E' g

day16 :: IO ()
day16 = do
    xs <- slurpLines "day16.txt"
    let (_, g) = parseGrid xs
    let (answer1, answer2) = solve g
    print $ "part 1: " ++ (show answer1)
    print $ "part 2: " ++ (show answer2)
