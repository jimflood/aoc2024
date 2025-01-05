
module Day21
    ( day21
    ) where

import Lib (parseGrid, slurpLines, Coordinate)
import qualified Data.Map as Map
import Data.List.Split (endBy)
import Data.List (group, groupBy, sort, sortBy)
import Data.Ord (comparing)
import Data.Function (on)

import Debug.Trace

type KeypadMap = Map.Map Char Coordinate

move :: KeypadMap -> (Char, Char) -> [String]
move km (a, b) = move' (km Map.! a) (km Map.! b)
    where
        move' (ax, ay) (bx, by) = move'' (bx - ax, by - ay)
            where
                --    +---+---+---+
                --    | 7 | 8 | 9 |
                --    +---+---+---+
                --    | 4 | 5 | 6 |
                --    +---+---+---+
                --    | 1 | 2 | 3 |
                --    +---+---+---+
                --        | 0 | A |
                --        +---+---+
                --
                --        +---+---+
                --        | ^ | A |
                --    +---+---+---+
                --    | < | v | > |
                --    +---+---+---+
                move'' (-2, -2)
                    | b == '4' = ["^^<<A"]
                    | otherwise = take 1 ["<<^^A", "^^<<A"]
                move'' (-2, -1)
                    | b == '1' = take 1 ["^<<A", "<<^A"]
                    | otherwise = drop 1 ["^<<A", "<<^A"]
                move'' (-2, 1)
                    | b == '<' = ["v<<A"]
                    | otherwise = take 1 ["<<vA", "v<<A"]
                move'' (-1, -2)
                    | b == '4' = ["^^<A"]
                    | otherwise = drop 1 ["^^<A", "<^^A"]
                move'' (-1, -1)
                    | b == '1' = ["^<A"]
                    | otherwise = take 1 ["<^A", "^<A"]
                move'' (-1, 0) = ["<A"]
                move'' (-1, 1)
                    | b == '<' = ["v<A"]
                    | otherwise = take 1 ["<vA", "v<A"]
                move'' (0, -3) = ["^^^A"]
                move'' (0, -2) = ["^^A"]
                move'' (0, -1) = ["^A"]
                move'' (0, 0) = ["A"]
                move'' (0, 1) = ["vA"]
                move'' (0, 2) = ["vvA"]
                move'' (0, 3) = ["vvvA"]
                move'' (1, -2) = take 1 ["^^>A", ">^^A"]
                move'' (1, -1)
                    | b == '^' = [">^A"]
                    | otherwise = take 1 ["^>A", ">^A"]
                move'' (1, 0) = [">A"]
                move'' (1, 1)
                    | b == '0' = [">vA"]
                    | otherwise = drop 1 [">vA", "v>A"]
                move'' (1, 2)
                    | b == '0' =  [">vvA"]
                    | otherwise = drop 1 [">vvA", "vv>A"]
                move'' (2, -1)
                    | b == 'A' = [">>^A"]
                    | otherwise = take 1 ["^>>A", ">>^A"]
                move'' (2, 0) = [">>A"]
                move'' (2, 1)
                    | b == 'A' = [">>vA"]
                    | otherwise = take 1 ["v>>A", ">>vA"]
                move'' (dx, dy) = trace (show a ++ " " ++ show b ++ " " ++ show (dx, dy)) error "???"

kmnum :: KeypadMap
kmnum = Map.fromList $ map ( \ (a, b) -> (b, a)) $ Map.toList $ snd $ parseGrid ["789", "456", "123", " 0A"]

kmdir :: KeypadMap
kmdir = Map.fromList $ map ( \ (a, b) -> (b, a)) $ Map.toList $ snd $ parseGrid [" ^A", "<v>"]

presses :: KeypadMap -> String -> [String]
presses km cs = comb $ map ( \ p -> move km p) (zip ('A' : cs) (cs))
    where
        comb (xs : []) = xs
        comb (xs : ys : zs) = comb ([x ++ y | x <- xs, y <- ys] : zs)
        comb _ = error "Ruh-roh"

numPresses :: String -> [String]
numPresses = presses kmnum

dirPresses :: String -> [String]
dirPresses = presses kmdir

keysOf :: String -> [String]
keysOf cs = map ( \ a -> a ++ "A") $ endBy "A" cs

keyTuplesOf :: String -> [(String, Int)]
keyTuplesOf cs = map ( \ a -> (head a, length a)) $ group $ sort $ keysOf cs

keyGraph :: String -> Int -> [[(String, Int)]]
keyGraph cs = keyGraph' $ map keyTuplesOf (numPresses cs)
    where
        keyGraph' tss n
            | n == 0 = tss
            | otherwise = keyGraph' (concatMap keyGraph'' tss) (n - 1)
            where
                keyGraph'' ts = tplit $ tcomb $ map ( \ t -> map ( \ a -> (a, snd t)) (dirPresses (fst t))) ts
                tplit xss = map condense $ map ( \ xs -> concatMap ( \ x -> map ( \ k -> (k, snd x)) (keysOf (fst x))) xs) xss
                condense ts = map ( \ x -> (fst (head x), sum (map snd x))) $ (groupBy ((==) `on` fst) $ sortBy (comparing fst) ts)
                tcomb = tcomb' [[]]
                    where
                        tcomb' acc [] = acc
                        tcomb' acc (xs : xss) = tcomb' [x : a | x <- xs, a <- acc] xss

lengthOf :: [(String, Int)] -> Int
lengthOf rs = sum $ map ( \ (k, v) -> (length k) * v) rs

solve :: [String] -> Int -> Int
solve xs n = sum $ zipWith (*) lengths codes
    where
        lengths = map (lengthOf . head) $ map ( \ a -> keyGraph a n) xs
        codes = map ( \ x -> read (init x) :: Int) xs

day21 :: IO ()
day21 = do
    xs <- slurpLines "day21.txt"
    let answer1 = solve xs 2
    print $ "part 1: " ++ (show answer1)
    let answer2 = solve xs 25
    print $ "part 2: " ++ (show answer2)
