{-# LANGUAGE RecordWildCards, MultiWayIf #-}

module Game where

import           Data.List
import           Data.List.Split
import           Control.Monad.Random
import           System.Random.Shuffle

--------------
-- MEASURES --
--------------

-- Introduce a measure for liquid type constraints.
{-@ measure llen @-}
llen :: [a] -> Int
llen []       = 0
llen (x : xs) = 1 + llen xs

-----------
-- TYPES --
-----------

-- Coordinates, used in mine generation and player interaction.
-- {-@ type Coord = { p : (Int, Int) | fst p >= 0 && snd p >= 0 } @-}
type Coord = (Int, Int)

-- # of mines.
{-@ type MineCount = { v : Int | 0 <= v && v <= 8 } @-}
type MineCount = Int

-- A board dimension (boards are (size x size) in area).
{-@ type Size = { v : Int | 0 < v } @-}
type Size = Int

----------------
-- DATA TYPES --
----------------

-- What the cell contains (either a mine or a count of how many mines are
-- nearby).
data CellFill
    = Mine
    | Proximity MineCount
    deriving (Eq)

instance Show CellFill where
    show Mine          = "*"
    show (Proximity c) = if c == 0 then " " else show c

-- The player's interaction with the cell.
data CellState
    = Undisturbed   -- no interaction
    | Flagged       -- player flag
    | Revealed      -- revealed mine or number
    deriving (Eq)

-- A cell, which combines a fill and a state.
data Cell = Cell { fill  :: CellFill
                 , state :: CellState }
                 deriving (Eq)

newtype Row = Row [Cell]     -- A row of cells on the board.
newtype Board = Board [Row]  -- A collection of rows.

-- Whether we're looking at this data as the player (potentially censored) or
-- as the computer (full access).
data Perspective = Player | Computer

---------------------------
-- CLASSES AND INSTANCES --
---------------------------

-- A class like Show, except which takes a Perspective as argument.
-- This is used for printing cells.
class PerspShow a where
    perspShow :: Perspective -> a -> String

instance PerspShow Cell where
    perspShow persp Cell {..} = case state of
        Undisturbed -> case persp of
            Player   -> "#"
            Computer -> show fill
        Flagged  -> "^"
        Revealed -> show fill

instance PerspShow Row where
    perspShow persp (Row cs) = concatMap (perspShow persp) cs

instance PerspShow Board where
    perspShow persp (Board rs) = unlines (map (perspShow persp) rs)

-- Regular show assumes we're showing to the player.
instance Show Cell where
    show = perspShow Player

instance Show Row where
    show = perspShow Player

instance Show Board where
    show = perspShow Player

------------------------------
-- CONVENIENCE CONSTRUCTORS --
------------------------------

mkFreshMineCell :: Cell
mkFreshMineCell = Cell { fill = Mine, state = Undisturbed }

{-@ mkFreshProxCell :: MineCount -> Cell @-}
mkFreshProxCell :: MineCount -> Cell
mkFreshProxCell p = Cell { fill = Proximity p, state = Undisturbed }

-----------------
-- BOARD SETUP --
-----------------

-- Generate a list of coordinates, used for selecting mine locations.
-- {-@ generateCoords :: w : Size -> h : Size -> { cs : [Coord] | llen cs == (w * h) } @-}
generateCoords :: Size -> Size -> [Coord]
generateCoords w h = [ (x, y) | x <- [0 .. w], y <- [0 .. h] ]

-- Select the coordinates that will be laid with mines.
{-@ chooseMines :: MonadRandom m => Size -> [Coord] -> m [Coord] @-}
chooseMines :: MonadRandom m => Size -> [Coord] -> m [Coord]
chooseMines n coords = sort . take n <$> shuffleM coords

makeBoard :: Int -> Size -> Size -> Board
makeBoard mc w h =


-- {-@ mkMaybeCell :: [Coord] -> Coord -> Maybe Cell @-}
-- mkMaybeCell :: [Coord] -> Coord -> Maybe Cell
-- mkMaybeCell []       coord = Nothing
-- mkMaybeCell (c : cs) coord = if
--     | c == coord -> Just mkMineCell
--     | otherwise  -> mkMaybeCell cs coord

-- {-@ groupN :: w : Size -> h : Size -> { xs : [a] | llen xs == w * h } -> [[a]] @-}
-- groupN :: Int -> Int -> [a] -> [[a]]
-- groupN w _ = chunksOf w

-- {-@ mkMineBoard :: Size -> Size -> [Coord] -> [Maybe Cell] @-}
-- mkMineBoard :: Size -> Size -> [Coord] -> [Maybe Cell]
-- mkMineBoard w h mines = map (mkMaybeCell mines) (generateCoords w h)

-- {-@ makeBoard :: { w : Int | w > 0 } -> { h : Int | h > 0 } -> [[Cell]] @-}
-- makeBoard :: Int -> Int -> [[Cell]]
-- makeBoard w h = replicate w $ replicate h mkMineCell
