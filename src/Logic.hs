{-
module Logic where

import Control.Comonad
import Control.Comonad.Representable.Store
import Data.Bool
import Data.Bool.HT
import Data.Distributive
import Data.Functor.Compose
import Data.Functor.Identity
import Data.Functor.Rep
import Data.Vector

data Cell = Alive | Dead deriving stock (Eq, Show)

type Point = (Int, Int)

type Grid = Store (Compose Vector Vector) Cell

type Rule = Grid -> Cell

instance Representable Vector where
  type Rep Vector = Int
  index v ((`mod` gridSize) -> i) = v ! i
  tabulate = generate gridSize

instance Distributive Vector where
  distribute = distributeRep

gridSize = 20

neighbors :: [Point]
neighbors = [(x, y) | x <- domain, y <- domain, (x, y) /= (0, 0)]
  where
    domain = [(-1) .. 1]

(+++) :: Point -> Point -> Point

infixl 8 +++

(a, b) +++ (c, d) = (a + c, b + d)

at :: [Point] -> Point -> [Point]
at coords origin = (+++) origin <$> coords

defaultRule :: Rule
defaultRule g = aliveNeighbors == requiredAliveNeighbors ?: (Alive, Dead)
  where
    aliveNeighbors =
      Prelude.length $
        Prelude.filter isAlive $
          experiment (at neighbors) g
    requiredAliveNeighbors = case extract g of
      Alive -> 2
      _ -> 3

isAlive Alive = True
isAlive _ = False

mkGrid :: [Point] -> Grid
mkGrid xs =
  store
    ((?: (Alive, Dead)) . (`Prelude.elem` xs))
    (0, 0)

render :: Grid -> String
render (StoreT (Identity (Compose g)) _) = Prelude.foldMap ((Prelude.++ "\n") . Prelude.foldMap (bool "." "#" . isAlive)) g
-}

module Logic
  ( mkGrid
  , basicRule
  , step
  , render
  , beacon
  , glider
  , blinker
  , at
  , Grid
  , Rule
  ) where

import Data.Functor.Compose (Compose(..))
import Data.Vector (Vector, (!), generate)
import Data.Bool (bool)
import Data.Distributive (Distributive(..))
import Data.Functor.Rep (Representable(..), distributeRep)
import Data.Functor.Identity (Identity(..))
import Control.Comonad.Representable.Store (Store(..), StoreT(..), store, experiment)
import Control.Comonad (Comonad(..))

type Coord = (Int, Int)
type Grid = Store (Compose Vector Vector) Bool
type Rule = Grid -> Bool

instance Distributive Vector where
  distribute = distributeRep

instance Representable Vector where
  type Rep Vector = Int
  index v i = v ! (i `mod` gridSize)
  tabulate = generate gridSize

gridSize :: Int
gridSize = 20

neighbourCoords :: [Coord]
neighbourCoords = [(x, y) | x <- [-1, 0, 1], y <- [-1, 0, 1], (x, y) /= (0, 0)]

addCoords :: Coord -> Coord -> Coord
addCoords (x, y) (x', y') = (x + x', y + y')

basicRule :: Rule
basicRule g = numNeighboursAlive == 3 || (alive && numNeighboursAlive == 2)
  where
    alive = extract g
    neighbours = experiment (at neighbourCoords) g
    numNeighboursAlive = length (filter id neighbours)

step :: Rule -> Grid -> Grid
step = extend

render :: Grid -> String
render (StoreT (Identity (Compose g)) _) = foldMap ((++ "\n") . foldMap (bool "." "#")) g

mkGrid :: [Coord] -> Grid
mkGrid xs = store (`elem` xs) (0, 0)

at :: [Coord] -> Coord -> [Coord]
coords `at` origin = map (addCoords origin) coords

glider, blinker, beacon :: [Coord]
glider = [(1, 0), (2, 1), (0, 2), (1, 2), (2, 2)]
blinker = [(0, 0), (1, 0), (2, 0)]
beacon = [(0, 0), (1, 0), (0, 1), (3, 2), (2, 3), (3, 3)]
