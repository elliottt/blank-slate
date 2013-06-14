{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Graphics.BlankSlate.Prim.Texture (
    Texture(..)
  , newTexture, newTextures
  , freeTexture, freeTextures
  , textureImage2d
  , PixelFormat()
  ) where

import Codec.Picture.Types
    (Image(..),Pixel(..),Pixel8,PixelF,PixelYA8,PixelRGB8,PixelRGBA8,PixelRGBF)
import Data.Vector.Storable (unsafeWith)
import Foreign (allocaArray,peekArray,withArrayLen)
import Graphics.Rendering.OpenGL.Raw.ARB.Compatibility
import Graphics.Rendering.OpenGL.Raw.Core31
import qualified Control.Exception as X


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

-- | Turn a JuicyPixels 'Image' into an OpenGL textures.
textureImage2d :: (PixelFormat a)
               => Texture
               -> Image a
               -> Int      -- ^ MIP map level
               -> Int      -- ^ Border (0 or 1)
               -> IO ()
textureImage2d tex image level border = do
  glBindTexture gl_TEXTURE_2D (getTexture tex)
  unsafeWith (imageData image) body `X.finally` glBindTexture gl_TEXTURE_2D 0
  where
  body = glTexImage2D
      gl_TEXTURE_2D
      (fromIntegral level)
      (pixelInternalFormat image)
      (fromIntegral (imageWidth  image))
      (fromIntegral (imageHeight image))
      (fromIntegral border)
      (pixelFormat image)
      (pixelType image)

class Pixel a => PixelFormat a where
  pixelInternalFormat :: Image a -> GLint
  pixelInternalFormat  = fromIntegral . pixelFormat
  pixelFormat         :: Image a -> GLenum
  pixelType           :: Image a -> GLenum

instance PixelFormat Pixel8 where
  pixelFormat _ = gl_LUMINANCE
  pixelType _   = gl_UNSIGNED_BYTE

instance PixelFormat PixelF where
  pixelFormat _ = gl_LUMINANCE
  pixelType _   = gl_FLOAT

instance PixelFormat PixelYA8 where
  pixelFormat _ = gl_LUMINANCE_ALPHA
  pixelType _   = gl_UNSIGNED_BYTE

instance PixelFormat PixelRGB8 where
  pixelFormat _ = gl_RGB
  pixelType _   = gl_UNSIGNED_BYTE

instance PixelFormat PixelRGBF where
  pixelFormat _ = gl_RGB
  pixelType _   = gl_FLOAT

instance PixelFormat PixelRGBA8 where
  pixelFormat _ = gl_RGBA
  pixelType _   = gl_UNSIGNED_BYTE
