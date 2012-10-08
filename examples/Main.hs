module Main where

import Graphics.BlankSlate

import Control.Exception (bracket)
import Data.Array (Array,listArray)
import Data.Array.Storable (StorableArray,thaw)


verts :: Array Int Double
verts = listArray (0,12)
  [  0.75,  0.75, 0.0, 1.0
  ,  0.75, -0.75, 0.0, 1.0
  , -0.75, -0.75, 0.0, 1.0
  ]

main :: IO ()
main  = withGraphics "Test" 640 480
      $ bracket newVertexShader freeShader
      $ \ shader -> do

        loadShaderFromFile shader "examples/vert.glsl"

        -- ship the triangle data to the card
        traingle     <- genBuffer
        triangleData <- thaw verts
        bindBuffer gl_ARRAY_BUFFER triangle
        bufferData gl_ARRAY_BUFFER triangleData gl_STATIC_DRAW
        unbindBuffer gl_ARRAY_BUFFER

        forever $ do

          clear gl_COLOR_BUFFER_BIT

          bindBuffer gl_ARRAY_BUFFER triangle
          drawArrays gl_TRIANGLES 0 3
          unbindBuffer gl_ARRAY_BUFFER

          flush
