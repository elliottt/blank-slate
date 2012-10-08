module Main where

import Graphics.BlankSlate

import Control.Exception (bracket)
import Control.Monad (forever)
import Data.Array (Array,listArray)
import Data.Array.Storable (thaw)

import Graphics.Rendering.OpenGL.Raw.ARB.Compatibility
import Graphics.Rendering.OpenGL.Raw.Core21


verts :: Array Int Double
verts = listArray (0,11)
  [  0.75,  0.75, 0.0, 1.0
  ,  0.75, -0.75, 0.0, 1.0
  , -0.75, -0.75, 0.0, 1.0
  ]

main :: IO ()
main  = withGraphics "Test" 640 480
      $ \ win    -> bracket newVertexShader freeShader
      $ \ shader -> do

        loadShaderFromFile shader "examples/vert.glsl"

        -- ship the triangle data to the card
        triangle     <- genBuffer
        triangleData <- thaw verts
        bindBuffer gl_ARRAY_BUFFER triangle
        bufferData gl_ARRAY_BUFFER triangleData gl_STATIC_DRAW
        unbindBuffer gl_ARRAY_BUFFER

        glEnableClientState gl_VERTEX_ARRAY
        glEnableClientState gl_COLOR_ARRAY

        forever $ do

          glClear (fromIntegral gl_COLOR_BUFFER_BIT)

          glMatrixMode gl_MODELVIEW
          glLoadIdentity

          bindBuffer gl_ARRAY_BUFFER triangle
          glDrawArrays gl_TRIANGLES 0 3
          unbindBuffer gl_ARRAY_BUFFER

          flush win
