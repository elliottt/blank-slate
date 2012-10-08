module Main where

import Graphics.BlankSlate

import Control.Exception (bracket)


main :: IO ()
main  = withGraphics "Test" 640 480
      $ bracket newVertexShader freeShader
      $ \ shader -> do
        loadShaderFromFile shader "examples/vert.glsl"
        putStrLn "compiled!"
