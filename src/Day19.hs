
module Day19
    ( day19
    ) where

import Lib (opchunk, slurpLines)
import qualified Data.Map as Map
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BSU
import Data.List.Split (endBy)
import Data.Tuple.Ops (app2, mapPolyT)

type Towel = BS.ByteString

type Pattern = BS.ByteString

type CacheMap = Map.Map BS.ByteString Int

parse :: [String] -> ([Towel], [Pattern])
parse = parse' . opchunk
    where
        parse' [[x], ys] = mapPolyT (map BSU.fromString) (endBy ", " x, ys)
        parse' _ = error "Nope"

count :: [Towel] -> (CacheMap, Int) -> Pattern -> (CacheMap, Int)
count ts = count'
    where
        count' (m, acc) p
            | Map.member p m = (m, acc + (m Map.! p))
            | otherwise = cache (foldl count'' (m, acc) ts)
            where
                cache (m1, acc1) = (Map.insert p (acc1 - acc) m1, acc1)
                count'' (mm, a) b
                    | b == p = (mm, a + 1)
                    | otherwise = count''' (BS.stripPrefix b p)
                    where
                        count''' Nothing = (mm, a)
                        count''' (Just np) = count' (mm, a) np

countsOf :: [Towel] -> [Pattern] -> [Int]
countsOf ts ps = snd $ foldl ( \ (m, cs) p -> app2 (:cs) (count ts (m, 0) p)) (Map.empty, []) ps

day19 :: IO ()
day19 = do
    xs <- slurpLines "day19.txt"
    let (ts, ps) = parse xs
    let cs = countsOf ts ps
    let answer1 = length $ filter (>0) cs
    print $ "part 1: " ++ (show answer1)
    let answer2 = sum cs
    print $ "part 2: " ++ (show answer2)
