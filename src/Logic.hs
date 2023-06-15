module Logic
  ( mkGrid,
    basicRule,
    render,
    beacon,
    glider,
    blinker,
    at,
    Grid,
    Rule,
  )
where

import Control.Comonad (Comonad (..))
import Control.Comonad.Representable.Store (Store (..), StoreT (..), experiment, store)
import Data.Bool (bool)
import Data.Bool.HT
import Data.Distributive (Distributive (..))
import Data.Functor.Compose (Compose (..))
import Data.Functor.Identity (Identity (..))
import Data.Functor.Rep (Representable (..), distributeRep)
import Data.Vector (Vector, generate, (!))

data CellState = Alive | Dead deriving stock (Eq, Show)

type Coord = (Int, Int)

type Grid = Store (Compose Vector Vector) CellState

type Rule = Grid -> CellState

instance Distributive Vector where
  distribute = distributeRep

instance Representable Vector where
  type Rep Vector = Int
  index v i = v ! (i `mod` gridSize)
  tabulate = generate gridSize

gridSize :: Int
gridSize = 20

neighborCoords :: [Coord]
neighborCoords =
  [ (x, y)
    | x <- [-1, 0, 1],
      y <- [-1, 0, 1],
      (x, y) /= (0, 0)
  ]

addCoords :: Coord -> Coord -> Coord
addCoords (x, y) (x', y') = (x + x', y + y')

basicRule :: Rule
basicRule g = isAlive g ?: (Alive, Dead)
  where
    isAlive g =
      numNeighborsAlive == 3
        || (extract g == Alive && numNeighborsAlive == 2)
    numNeighborsAlive =
      length $
        filter (== Alive) $
          experiment (at neighborCoords) g

render :: Grid -> String
render (StoreT (Identity (Compose g)) _) = foldMap ((++ "\n") . foldMap (bool "." "#" . (== Alive))) g

mkGrid :: [Coord] -> Grid
mkGrid xs = store (bool Dead Alive . (`elem` xs)) (0, 0)

at :: [Coord] -> Coord -> [Coord]
coords `at` origin = addCoords origin <$> coords

glider, blinker, beacon :: [Coord]
glider = [(1, 0), (2, 1), (0, 2), (1, 2), (2, 2)]
blinker = [(0, 0), (1, 0), (2, 0)]
beacon = [(0, 0), (1, 0), (0, 1), (3, 2), (2, 3), (3, 3)]
