{-# LANGUAGE ForeignFunctionInterface #-}

module Graphics.BlankSlate.Texture (
    Texture()
  , newTexture, newTextures
  , freeTexture, freeTextures

  , loadTexture
  , surfaceToTexture
  ) where

import Control.Monad ((<=<))
import Foreign (Ptr,allocaArray,peekArray,withArrayLen,withForeignPtr)
import Foreign.C.Types (CInt(..),CUInt(..))
import Graphics.UI.SDL
import Graphics.UI.SDL.Image
import Graphics.Rendering.OpenGL.Raw.Core21


newtype Texture = Texture
  { getTexture :: GLuint
  } deriving (Show)

-- | Allocate a texture.
newTexture :: IO Texture
newTexture  = do
  texs <- newTextures 1
  case texs of
    [tex] -> return tex
    _     -> fail "newTexture: invalid number of textures"

-- | Allocate many textures at once.
newTextures :: Int -> IO [Texture]
newTextures n = allocaArray n $ \ ptr -> do
  glGenTextures (fromIntegral n) ptr
  map Texture `fmap` peekArray n ptr

-- | Free a texture.
freeTexture :: Texture -> IO ()
freeTexture tex = freeTextures [tex]

-- | Free many textures.
freeTextures :: [Texture] -> IO ()
freeTextures texs =
    withArrayLen (map getTexture texs) (glDeleteTextures . fromIntegral)


-- | Load an OpenGL texture from a file.
loadTexture :: FilePath -> IO Texture
loadTexture  = surfaceToTexture <=< load

-- | Convert an SDL surface into an OpenGL texture.
surfaceToTexture :: Surface -> IO Texture
surfaceToTexture suf = do
  tex <- newTexture
  withForeignPtr suf $ \ sufP -> do
    res <- c_SDL_Surface_to_glTextureObject sufP (fromIntegral (getTexture tex))
    case res of
      0 -> return tex
      1 -> fail "surfaceToTexture: Invalid surface"
      _ -> fail "surfaceToTexture: Unexpected error"

foreign import ccall unsafe "sdl-opengl.h SDL_Surface_to_glTextureObject"
  c_SDL_Surface_to_glTextureObject :: Ptr SurfaceStruct -> CUInt -> IO CInt
