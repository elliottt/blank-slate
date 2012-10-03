module Main where

import Graphics.BlankSlate

import Control.Concurrent (threadDelay)


main :: IO ()
main  = withGraphics "Test" 640 480 (threadDelay 1000000)
