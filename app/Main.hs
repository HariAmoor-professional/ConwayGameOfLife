{-
import Logic
import Prelude

import Control.Monad (forM_)
import Control.Comonad
import Control.Concurrent

glider, blinker, beacon :: [Point]
glider = [(1, 0), (2, 1), (0, 2), (1, 2), (2, 2)]
blinker = [(0, 0), (1, 0), (2, 0)]
beacon = [(0, 0), (1, 0), (0, 1), (3, 2), (2, 3), (3, 3)]

start :: Grid
start = mkGrid $ (glider `at` (0, 0)) ++ (beacon `at` (75, 25))

main :: IO ()
main = forM_ (iterate (extend defaultRule) start) $ \grid -> do
  putStr "\ESC[2J"
  putStrLn $ Logic.render grid
-}


{-# language TypeFamilies #-}
module Main where

import Logic
import Control.Concurrent (threadDelay)
import Control.Monad (forM_)

tickTime :: Int
tickTime = 200000

start :: Grid
start = mkGrid $
     glider `at` (0, 0)
  ++ beacon `at` (15, 5)

main :: IO ()
main = forM_ (iterate (step basicRule) start) $ \grid -> do
  putStr "\ESC[2J" -- Clear terminal screen
  putStrLn (render grid)
  threadDelay tickTime

