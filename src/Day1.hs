
module Day1
    ( day1
    ) where

import Lib (slurpLines)

import qualified Data.List as List
import Text.Regex.Posix

parse :: [String] -> [(Int, Int)]
parse = map (parse'' . parse')
    where
        parse' :: String -> [[String]]
        parse' a = a =~ "([0-9]+) +([0-9]+)" :: [[String]]
        parse'' [[_, y, z]] = ((read y :: Int), (read z :: Int))
        parse'' _ = error "bad input"

leftRight :: [(Int, Int)] -> ([Int], [Int])
leftRight xs = (map fst xs, map snd xs)

part1 :: ([Int], [Int]) -> Int
part1 (xs, ys) = sum $ map ( \ (x, y) -> abs (x - y)) $ zip (List.sort xs) (List.sort ys)

part2 :: ([Int], [Int]) -> Int
part2 (xs, ys) = sum $ map ( \ x -> x * (length $ filter (==x) ys)) xs

day1 :: IO ()
day1 = do
    xs <- slurpLines "day1.txt"
    let answer1 = (part1 . leftRight . parse) xs
    print $ "part 1: " ++ (show answer1)
    let answer2 = (part2 . leftRight . parse) xs
    print $ "part 2: " ++ (show answer2)
