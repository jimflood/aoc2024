
module Day18
    ( day18
    ) where

import Lib (slurpLines, Coordinate, Grid, drawGrid)
import Data.List.Split (splitOn)
import qualified Data.Map as Map
import Data.Maybe

type StepMap = Map.Map Coordinate (Maybe Int)

parse :: [String] -> [Coordinate]
parse = map (parse' . (splitOn ","))
    where
        parse' [a, b] = (read a, read b)
        parse' _ = error "ruh-roh"

emptyStepMap :: (Int, Int) -> StepMap
emptyStepMap (w, h) = Map.fromList [((x, y), Nothing) | x <- [0..w - 1], y <- [0..h - 1]]

dropByte :: StepMap -> Coordinate -> StepMap
dropByte sm p = Map.insert p (Just (-1)) sm

takeSteps :: Int -> (StepMap, [Coordinate]) -> (StepMap, [Coordinate])
takeSteps n (sm, ps) = foldl takeSteps' (sm, []) (concatMap fourDirections ps)
    where
        takeSteps' :: (StepMap, [Coordinate]) -> Coordinate -> (StepMap, [Coordinate])
        takeSteps' a b
            | Map.notMember b (fst a) = a
            | isJust ((fst a) Map.! b) = a
            | otherwise = (Map.insert b (Just (n + 1)) (fst a), b : (snd a))

fourDirections :: Coordinate -> [Coordinate]
fourDirections (x, y) = [(x, y - 1), (x, y + 1), (x - 1, y), (x + 1, y)]

start :: Coordinate
start = (0, 0)

initial :: (Int, Int) -> [Coordinate] -> Int -> StepMap
initial wh bs i = Map.insert start (Just 0) (foldl dropByte (emptyStepMap wh) (take i bs))

solve :: (Int, Int) -> [Coordinate] -> Int -> StepMap
solve wh bs i = solve' 0 (initial wh bs i, [start])
    where
        solve' _ (sm, []) = sm
        solve' n s = solve' (n + 1) (takeSteps n s)

manySolve :: (Int, Int) -> [Coordinate] -> (Coordinate, StepMap)
manySolve (w, h) bs = manySolve' (length bs)
    where
        manySolve' i = manySolve'' (solve (w, h) bs i)
            where
                manySolve'' sm
                    | isJust (sm Map.! (w - 1, h - 1)) = (head (drop i bs), sm)
                    | otherwise = manySolve' (i - 1)

bgOf :: StepMap -> Maybe Coordinate -> Grid
bgOf sm mp = Map.mapWithKey bgOf' sm
    where
        bgOf' _ Nothing = '.'
        bgOf' _ (Just (-1)) = '#'
        bgOf' k (Just _)
            | (Just k) == mp = '@'
            | otherwise = 'O'

day18 :: IO ()
day18 = do
    xs <- slurpLines "day18.txt"
    let bs = parse xs
    let wh = (71, 71)
    let sm = solve wh bs 1024
    let answer1 = sm Map.! (70, 70)
    print $ "part 1: " ++ (show answer1)
    let (p@(x, y), nsm) = manySolve wh bs
    putStr (drawGrid (wh, (bgOf nsm (Just p))))
    let answer2 = show x ++ "," ++ show y
    print $ "part 2: " ++ (show answer2)
