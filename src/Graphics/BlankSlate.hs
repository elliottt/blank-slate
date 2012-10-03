module Graphics.BlankSlate (
    module Exports

  , withGraphics
  ) where

import Graphics.BlankSlate.Shader  as Exports
import Graphics.BlankSlate.Texture as Exports

import Graphics.UI.SDL as SDL
import Graphics.Rendering.GLU.Raw
import Graphics.Rendering.OpenGL.Raw.Core21
import Graphics.Rendering.OpenGL.Raw.ARB.Compatibility


withGraphics :: String -> Int -> Int -> IO a -> IO a
withGraphics title width height body = withInit [InitEverything] $ do
  -- SDL
  SDL.setCaption title ""
  _ <- SDL.setVideoMode width height 24 [OpenGL]

  -- OpenGL
  glClearColor 0 0 0 1.0
  glClearDepth 1
  glDepthFunc  gl_LESS
  glShadeModel gl_SMOOTH

  glMatrixMode gl_PROJECTION
  glLoadIdentity
  gluPerspective 45 (fromIntegral width / fromIntegral height) 0.1 100

  glMatrixMode gl_MODELVIEW

  glEnable gl_TEXTURE_2D
  glEnable gl_BLEND
  glBlendFunc gl_SRC_ALPHA gl_ONE_MINUS_SRC_ALPHA

  glFlush

  body
