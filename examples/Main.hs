module Main where

import Graphics.BlankSlate
import Paths_blank_slate

import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Control.Monad (when)
import Data.Array (Array,listArray)
import Data.Array.Storable (thaw,withStorableArray)
import Foreign (nullPtr,castPtr)
import System.FilePath ((</>))
import System.Exit

-- for now
import Graphics.Rendering.OpenGL.Raw.ARB.Compatibility
import Graphics.Rendering.OpenGL.Raw.Core31
import qualified Graphics.UI.GLFW as GLFW


verts :: Array Int Float
verts = listArray (0,11)
  [  0.75,  0.75, 0.0, 1.0
  ,  0.75, -0.75, 0.0, 1.0
  , -0.75, -0.75, 0.0, 1.0
  ]

main :: IO ()
main  =
  withGraphics "Test" 640 480             $ \ win  ->
  bracket newProgram        deleteProgram $ \ pgm  ->
  bracket newVertexShader   deleteShader  $ \ vert ->
  bracket newFragmentShader deleteShader  $ \ frag -> do

    path <- getDataDir

    loadShaderFromFile vert (path </> "vert.glsl")
    loadShaderFromFile frag (path </> "frag.glsl")

    attachShader pgm vert
    attachShader pgm frag
    linkProgram pgm
    useProgram pgm

    -- ship the triangle data to the card
    triangle     <- genBuffer
    triangleData <- thaw verts
    bindBuffer gl_ARRAY_BUFFER triangle
    withStorableArray triangleData $ \ ptr ->
      glBufferData gl_ARRAY_BUFFER (12 * 4) (castPtr ptr) gl_STATIC_DRAW
    glVertexPointer 4 gl_FLOAT 0 nullPtr
    unbindBuffer gl_ARRAY_BUFFER

    glEnableClientState gl_VERTEX_ARRAY

    glMatrixMode gl_MODELVIEW
    glLoadIdentity

    glMatrixMode gl_PROJECTION
    glLoadIdentity

    let handleKey _win k _code _ks _mods =
            case k of
                GLFW.Key'Q -> exitSuccess
                _ -> return ()

        loop = do
          GLFW.pollEvents

          glClear (fromIntegral gl_COLOR_BUFFER_BIT)

          glMatrixMode gl_MODELVIEW
          glLoadIdentity

          bindBuffer gl_ARRAY_BUFFER triangle
          glDrawArrays gl_TRIANGLES 0 3
          unbindBuffer gl_ARRAY_BUFFER

          flush win

          threadDelay 1000

          close <- GLFW.windowShouldClose win
          when (not close) loop

    GLFW.setKeyCallback win $ Just handleKey
    loop
