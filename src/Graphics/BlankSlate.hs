module Graphics.BlankSlate (
    module Exports

  , withGraphics
  ) where

import Graphics.BlankSlate.Resource as Exports
import Graphics.BlankSlate.Texture  as Exports
import Graphics.BlankSlate.Window   as Exports

import Graphics.UI.SDL as SDL


withGraphics :: IO a -> IO a
withGraphics  = SDL.withInit [InitEverything]
