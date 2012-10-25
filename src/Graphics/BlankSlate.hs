module Graphics.BlankSlate (
    module Prim

  , Window(..)
  , withGraphics
  , flush
  ) where

import Graphics.BlankSlate.Prim.Buffer  as Prim
import Graphics.BlankSlate.Prim.Shader  as Prim
import Graphics.BlankSlate.Prim.Texture as Prim

import Graphics.UI.SDL as SDL
import Graphics.Rendering.GLU.Raw
import Graphics.Rendering.OpenGL.Raw.Core21
import Graphics.Rendering.OpenGL.Raw.ARB.Compatibility


newtype Window = Window { getWindow :: SDL.Surface }


withGraphics :: String -> Int -> Int -> (Window -> IO a) -> IO a
withGraphics title width height body = withInit [InitEverything] $ do
  -- SDL
  SDL.setCaption title ""
  win <- SDL.setVideoMode width height 24 [OpenGL]

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

  body (Window win)


flush :: Window -> IO ()
flush win = do
  glFlush
  SDL.glSwapBuffers
  SDL.flip (getWindow win)
