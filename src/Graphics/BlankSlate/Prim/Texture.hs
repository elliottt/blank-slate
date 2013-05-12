{-# LANGUAGE ForeignFunctionInterface #-}

module Graphics.BlankSlate.Prim.Texture (
    Texture(..)
  , newTexture, newTextures
  , freeTexture, freeTextures
  ) where

import Foreign (allocaArray,peekArray,withArrayLen)
import Graphics.Rendering.OpenGL.Raw.Core31


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
