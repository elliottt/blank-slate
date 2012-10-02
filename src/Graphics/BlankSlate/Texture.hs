{-# LANGUAGE ForeignFunctionInterface #-}

module Graphics.BlankSlate.Texture (
    Texture
  , loadTexture
  , surfaceToTexture
  ) where

import Graphics.BlankSlate.Resource

import Control.Monad ((<=<))
import Foreign (Ptr,allocaArray,peekArray,withArrayLen,withForeignPtr)
import Foreign.C.Types (CInt(..))
import Graphics.UI.SDL
import Graphics.UI.SDL.Image
import Graphics.Rendering.OpenGL.Raw.Core21


newtype Texture = Texture
  { getTexture :: GLuint
  } deriving (Show)

instance Resource Texture where

  genResources n = allocaArray n $ \ ptr -> do
    glGenTextures (fromIntegral n) ptr
    map Texture `fmap` peekArray n ptr

  genResource = do
    texs <- genResources 1
    case texs of
      [tex] -> return tex
      _     -> fail "genResource(Texture): invalid number of resources"

  freeResource tex = freeResources [tex]

  freeResources texs =
    withArrayLen (map getTexture texs) (glDeleteTextures . fromIntegral)


-- | Load an OpenGL texture from a file.
loadTexture :: FilePath -> IO Texture
loadTexture  = surfaceToTexture <=< load

-- | Convert an SDL surface into an OpenGL texture.
surfaceToTexture :: Surface -> IO Texture
surfaceToTexture suf = do
  tex <- genResource
  withForeignPtr suf $ \ sufP -> do
    res <- c_SDL_Surface_to_glTextureObject sufP (getTexture tex)
    case res of
      0 -> return tex
      1 -> fail "surfaceToTexture: Invalid surface"
      _ -> fail "surfaceToTexture: Unexpected error"

foreign import ccall unsafe "sdl-opengl.h SDL_Surface_to_glTextureObject"
  c_SDL_Surface_to_glTextureObject :: Ptr SurfaceStruct -> GLuint -> IO CInt
