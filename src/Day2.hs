
module Day2
    ( day2
    ) where

import Lib (slurpLines)

import Data.List.Split

parse :: [String] -> [[Int]]
parse = map (parse'' . parse')
    where
        parse' a = splitOn " " a
        parse'' xs = map ( \ x -> read x :: Int) xs

isSafe :: [Int] -> Bool
isSafe xs = isSafe' $ map ( \ (a, b) -> b - a) (zip xs (tail xs))
    where
        isSafe' :: [Int] -> Bool
        isSafe' ys = ((all (<0) ys) || (all (>0) ys)) && (all ( \ y -> y >= 1 && y <= 3) (map abs ys))

part1 :: [[Int]] -> Int
part1 xs = length $ filter id $ map isSafe xs

part2 :: [[Int]] -> Int
part2 xs =  length $ filter id $ map ( \ x -> any isSafe (alts x)) xs

alts :: [Int] -> [[Int]]
alts xs = alts' [xs] 0
    where
        alts' acc pos
            | pos == length xs = acc
            | otherwise = alts' (((take pos xs) ++ (drop (pos + 1) xs)) : acc) (pos + 1)

day2 :: IO ()
day2 = do
    xs <- slurpLines "day2.txt"
    let rules = parse xs
    let answer1 = part1 rules
    print $ "part 1: " ++ (show answer1)
    let answer2 = part2 rules
    print $ "part 2: " ++ (show answer2)
