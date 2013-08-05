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


withGraphics :: String -> Int -> Int -> (GLFW.Window -> IO a) -> IO a
withGraphics title width height body =
    do { check "GLFW.init" GLFW.init
       ; w <- GLFW.createWindow width height title Nothing Nothing
       ; case w of
           Nothing -> fail "Could not create GLFW window."
           Just win -> X.bracket (setup win) (cleanup win) (const (body win))
       }
  where

  check l m = do
    success <- m
    unless success (fail ("a call to ``" ++ l ++ "` failed"))

  setup win = do
    GLFW.makeContextCurrent (Just win)

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

  cleanup w () = do
    GLFW.destroyWindow w
    GLFW.terminate


flush :: GLFW.Window -> IO ()
flush win = do
  glFlush
  GLFW.swapBuffers win
