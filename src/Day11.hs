
module Day11
    ( day11
    ) where

import Lib (slurpLines)
import Data.List.Split (splitOn)
import qualified Data.Map as Map

type Engraving = Int

type Count = Int

type StoneMap = Map.Map Engraving Count

parse :: [String] -> StoneMap
parse (a : []) = Map.fromList $ zip (map read (splitOn " " a)) (cycle [1])
parse _ = error "bad input"

run :: Int -> StoneMap -> StoneMap
run n m
    | n == 0 = m
    | otherwise = run (n - 1) (Map.foldlWithKey change Map.empty m)
        where
            change :: StoneMap -> Engraving -> Count -> StoneMap
            change a k b
                | k == 0 = (add 1 a)
                | (not . even) len = (add (k * 2024) a)
                | otherwise = ((add kl) . (add kr)) a
                    where
                        str = show k
                        len = length str
                        mid = len `div` 2
                        kl = read $ take mid str
                        kr = read $ drop mid str
                        add :: Engraving -> StoneMap -> StoneMap
                        add kk mm = Map.alter add' kk mm
                            where
                                add' Nothing = Just b
                                add' (Just bb) = Just (bb + b)

day11 :: IO ()
day11 = do
    xs <- slurpLines "day11.txt"
    let m = parse xs
    let answer1 = sum $ Map.elems (run 25 m)
    print $ "part 1: " ++ (show answer1)
    let answer2 = sum $ Map.elems (run 75 m)
    print $ "part 2: " ++ (show answer2)

