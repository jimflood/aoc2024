
module Day5
    ( day5
    ) where

import Lib (slurpLines, Coordinate, Grid, parseGrid)
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.List.Split as Split
import Debug.Trace

type Rule = (Int, Int)

type Update = [Int]

chunk :: [String] -> [[String]]
chunk = chunk' [[]]
    where
        chunk' (a : acc) (x : xs)
            | x == "" = chunk' ([] : (a : acc)) xs
            | otherwise = chunk' ((x : a) : acc) xs
        chunk' acc [] = acc

parse :: [String] -> ([Update], [Rule])
parse = parse' . chunk
    where
        parse' [a, b] = (map intsOf a, map tuplify (map intsOf b))
            where
                intsOf xs = map read (Split.splitOneOf "|," xs)
                tuplify [x, y] = (x, y)

valid :: [Rule] -> Update -> Bool
valid [] u = True
valid ((a, b) : rs) u
    | check (List.elemIndex a u) (List.elemIndex b u) = valid rs u
    | otherwise = False
    where
        check (Just x) (Just y) = x < y
        check _ _ = True

solve :: [Rule] -> [Update] -> Int
solve rs us = sum $ map mid $ filter ( \ u -> valid rs u) us
    where
        mid xs = head $ drop ((length xs) `div` 2) xs

invalids :: [Rule] -> [Update] -> [Update]
invalids rs us = filter ( \ u -> not (valid rs u)) us

bubble :: [Rule] -> Update -> Update
bubble rs = bubble' [] []
    where
        bubble' acc [] [] = acc
        bubble' acc ys (a : b : cs)
            | valid rs [a, b] = bubble' acc (a : ys) (b : cs)
            | otherwise = bubble' acc (b : ys) (a : cs)
        bubble' acc ys [c] = bubble' (c : acc) [] ys

-- I didn't think of using an ordering function -- so much simpler 🐰:
--bubble rs us = List.sortBy cmp us
--    where
--        cmp a b
--            | valid rs [a, b] = LT
--            | otherwise = GT

solve2 :: [Rule] -> [Update] -> Int
solve2 rs us = solve rs $ map ( \ x -> bubble rs x) (invalids rs us)

day5 :: IO ()
day5 = do
    xs <- slurpLines "day5.txt"
    let (us, rs) = parse xs
    let answer1 = solve rs us
    print $ "part 1: " ++ (show answer1)
    let answer2 = solve2 rs us
    print $ "part 2: " ++ (show answer2)
