module Main where

import Graphics.BlankSlate

import Control.Concurrent (threadDelay)


main = withGraphics $ do
  win <- initGraphics "Test" 640 480
  threadDelay 1000000
