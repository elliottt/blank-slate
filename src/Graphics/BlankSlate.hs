module Graphics.BlankSlate (
    module Prim

  , withGraphics
  , flush
  ) where

import Graphics.BlankSlate.Prim.Buffer  as Prim
import Graphics.BlankSlate.Prim.Shader  as Prim
import Graphics.BlankSlate.Prim.Texture as Prim

import Control.Monad (unless)
import Graphics.Rendering.GLU.Raw
import Graphics.Rendering.OpenGL.Raw.ARB.Compatibility
import Graphics.Rendering.OpenGL.Raw.Core31
import qualified Graphics.UI.GLFW as GLFW
import qualified Control.Exception as X


withGraphics :: String -> Int -> Int -> IO a -> IO a
withGraphics title width height body = X.bracket setup cleanup (const body)
  where

  check l m = do
    success <- m
    unless success (fail ("a call to ``" ++ l ++ "` failed"))

  setup = do

    -- GLFW
    check "GLFW.initialize"  GLFW.initialize

    check "GLFW.openWindow" $ GLFW.openWindow GLFW.defaultDisplayOptions
      { GLFW.displayOptions_width  = width
      , GLFW.displayOptions_height = height
      }

    GLFW.setWindowTitle title

    -- OpenGL
    glClearColor 0 0 0 0

    gluPerspective 45 (fromIntegral width / fromIntegral height) 0.1 100

    glMatrixMode gl_MODELVIEW
    glLoadIdentity

    glMatrixMode gl_PROJECTION
    glLoadIdentity

    glEnable gl_TEXTURE_2D
    glEnable gl_BLEND
    glBlendFunc gl_SRC_ALPHA gl_ONE_MINUS_SRC_ALPHA

    glFlush

  cleanup () = do
    GLFW.closeWindow
    GLFW.terminate


flush :: IO ()
flush  = do
  glFlush
  GLFW.swapBuffers
