module Graphics.BlankSlate.Window (
    Window
  , initGraphics
  ) where

import Graphics.UI.SDL as SDL


newtype Window = Window
  { getWindow :: SDL.Surface
  } deriving (Show)

initGraphics :: String -> Int -> Int -> IO Window
initGraphics title width height = do
  -- SDL
  SDL.setCaption title ""
  suf <- SDL.setVideoMode width height 24 [OpenGL]

  -- OpenGL

  return (Window suf)
