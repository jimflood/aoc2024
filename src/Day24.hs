
module Day24
    ( day24
    ) where

import Lib (opchunk, slurpLines)
import qualified Data.Map as Map
import Data.List.Split (splitOneOf)
import Data.List (intercalate, sort, sortBy)
import qualified Data.Bits as Bits

import Debug.Trace

type WireMap = Map.Map String Int

data Gate = Gate { ina :: String
                 , op :: String
                 , inb :: String
                 , out :: String
                 } deriving (Show)

parse :: [String] -> ([Gate], WireMap)
parse = parse' . opchunk
    where
        parse' (xs : ys : []) = (map gate ys, Map.fromList (map wire xs))
            where
                wire x = wire' $ splitOneOf ": " x
                    where
                        wire' (a : _ : c : []) = (a, read c :: Int)
                        wire' xx = trace (show xx) error "HALT"
                gate y = gate' $ splitOneOf " -> " y
                    where
                        gate' (a : b : c : ds) = Gate { ina = a, op = b, inb = c, out = last ds }
                        gate' _ = error "nope"
        parse' _ = error "nope"

step :: (String -> String) -> ([Gate], WireMap) -> ([Gate], WireMap)
step tr (gs, wm) = foldl step' ([], wm) gs
    where
        step' (gs2, wm2) g
            | Map.member (ina g) wm2 && Map.member (inb g) wm2 = (gs2, Map.insert (tr (out g)) res wm2)
            | otherwise = (g : gs2, wm2)
                where
                    res
                        | (op g) == "AND" = (wm2 Map.! (ina g)) Bits..&. (wm2 Map.! (inb g))
                        | (op g) == "OR" = (wm2 Map.! (ina g)) Bits..|. (wm2 Map.! (inb g))
                        | (op g) == "XOR" = (wm2 Map.! (ina g)) Bits..^. (wm2 Map.! (inb g))
                        | otherwise = error "nope"

cross :: String -> String
cross "hdt" = "z05"
cross "z05" = "hdt"
cross "gbf" = "z09"
cross "z09" = "gbf"
cross "nbf" = "z30"
cross "z30" = "nbf"
cross "jgt" = "mht"
cross "mht" = "jgt"
cross a = a

run :: (String -> String) -> ([Gate], WireMap) -> WireMap
run _ ([], wm) = wm
run tr (gs, wm) = run tr $ step tr (gs, wm)

solve :: WireMap -> Int
solve wm = foldl ( \ a b -> (a * 2) + b) 0 $ map snd $ sortBy (flip compare) $ filter ( \ (k, _) -> head k == 'z') (Map.toList wm)

solve2 :: [Gate] -> String
solve2 gs = intercalate "," $ sort $ filter ( \ a -> cross a /= a) $ map out gs

day24 :: IO ()
day24 = do
    xs <- slurpLines "day24.txt"
    let (gs, wm) = parse xs
    let r1 = run id (gs, wm)
    let answer1 = solve r1
    print $ "part 1: " ++ (show answer1)
    let answer2 = solve2 gs
    print $ "part 2: " ++ answer2
