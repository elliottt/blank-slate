module Main where

import Graphics.BlankSlate

import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Control.Monad (forever)
import Data.Array (Array,listArray)
import Data.Array.Storable (thaw)
import Foreign (nullPtr,with)

import Graphics.Rendering.OpenGL.Raw.ARB.Compatibility
import Graphics.Rendering.OpenGL.Raw.Core21


verts :: Array Int Float
verts = listArray (0,11)
  [  0.75,  0.75, 0.0, 1.0
  ,  0.75, -0.75, 0.0, 1.0
  , -0.75, -0.75, 0.0, 1.0
  ]

main :: IO ()
main  = withGraphics "Test" 640 480
      $ \ win  -> bracket newVertexShader freeShader
      $ \ vert -> bracket newFragmentShader freeShader
      $ \ frag -> do

        loadShaderFromFile vert "examples/vert.glsl"
        loadShaderFromFile frag "examples/frag.glsl"

        pgm <- glCreateProgram
        glAttachShader pgm (getShader vert)
        glAttachShader pgm (getShader frag)
        glLinkProgram pgm
        glUseProgram pgm

        -- ship the triangle data to the card
        triangle     <- genBuffer
        triangleData <- thaw verts
        bindBuffer gl_ARRAY_BUFFER triangle
        bufferData gl_ARRAY_BUFFER triangleData gl_STATIC_DRAW
        glVertexPointer 4 gl_FLOAT 0 nullPtr
        unbindBuffer gl_ARRAY_BUFFER

        glEnableClientState gl_VERTEX_ARRAY

        glMatrixMode gl_MODELVIEW
        glLoadIdentity

        glMatrixMode gl_PROJECTION
        glLoadIdentity

        forever $ do

          glClear (fromIntegral gl_COLOR_BUFFER_BIT)

          glMatrixMode gl_MODELVIEW
          glLoadIdentity

          bindBuffer gl_ARRAY_BUFFER triangle
          glDrawArrays gl_TRIANGLES 0 3
          unbindBuffer gl_ARRAY_BUFFER

          flush win

          threadDelay 100000
