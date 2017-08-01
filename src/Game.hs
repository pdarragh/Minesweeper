module Game where

import Control.Monad.Random
import Data.List (sort)

data BoardCell = Empty
               | Flag Int
               | Mine
               deriving (Show, Eq)

data ChoiceCell a = Chosen a
                  | Unchosen a
                  deriving (Show, Eq)

type Cell = ChoiceCell BoardCell

type Row   = [Cell]
type Board = [Row]

data Game = Game { board    :: Board
                 , finished :: Bool
                 , won      :: Bool
                 } deriving (Show, Eq)

createGame :: MonadRandom m => Int -> Int -> Int -> m Game
createGame x y m = do
    ms <- minePositions x y m
    return (Game (createBoard x y ms) False False)

createBoard :: Int -> Int -> [(Int, Int)] -> Board
createBoard width height ms = createBoard' width height 0 ms []

createBoard' :: Int -> Int -> Int -> [(Int, Int)] -> Board -> Board
createBoard' width height y ms rs
    | y == height = rs
    | otherwise = (createRow width y ms) : (createBoard' width height (y + 1) ms rs)

createRow :: Int -> Int -> [(Int, Int)] -> Row
createRow width y ms = createRow' width 0 y ms []

createRow' :: Int -> Int -> Int -> [(Int, Int)] -> Row -> Row
createRow' width x y ms ps
    | x == width = ps
    | otherwise = (createCell x y ms) : (createRow' width (x + 1) y ms ps)

createCell :: Int -> Int -> [(Int, Int)] -> Cell
createCell x y ms = Unchosen marker where
    marker = case surroundingMineCount x y ms of
               Just n -> if n == 0
                           then Empty
                           else Flag n
               Nothing -> Mine

surroundingMineCount :: Int -> Int -> [(Int, Int)] -> Maybe Int
surroundingMineCount x y ms
    | elem (x, y) ms = Nothing
    | otherwise = Just (length (filter (`elem` ms) (surroundingCells x y)))

surroundingCells :: Int -> Int -> [(Int, Int)]
surroundingCells x y = map (addPos (x, y)) [(-1, -1), (0, -1), (1, -1), (-1, 0), (1, 0), (-1, 1), (0, 1), (1, 1)]

addPos :: (Int, Int) -> (Int, Int) -> (Int, Int)
addPos (a, b) (c, d) = (a + c, b + d)

getPos :: MonadRandom m => Int -> m Int
getPos n = getRandomR (0, n - 1)

minePositions :: MonadRandom m => Int -> Int -> Int -> m [(Int, Int)]
minePositions x y m
    | m > (x * y) = error "too many mines for board size"
    | otherwise = do
        ps <- minePositions' x y m
        return (sort ps)

minePositions' :: MonadRandom m => Int -> Int -> Int -> m [(Int, Int)]
minePositions' _ _ 0 = return []
minePositions' x y m = do
    ps <- minePositions' x y (m - 1)
    x_pos <- getPos x
    y_pos <- getPos y
    let pos = (x_pos, y_pos)
    if elem pos ps
       then minePositions' x y m
       else return (pos : ps)
