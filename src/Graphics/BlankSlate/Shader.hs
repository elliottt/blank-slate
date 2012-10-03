{-# LANGUAGE EmptyDataDecls #-}

module Graphics.BlankSlate.Shader (
    SVertex
  , SFragment

  , Shader(..)
  , newVertexShader, newFragmentShader
  , freeShader
  ) where

import Graphics.Rendering.OpenGL.Raw.Core21


data SVertex
data SFragment

newtype Shader kind = Shader
  { getShader :: GLuint
  } deriving (Show)

-- | Allocate a new fragment shader.
newFragmentShader :: IO (Shader SFragment)
newFragmentShader  = newShader gl_FRAGMENT_SHADER

-- | Allocate a new vertex shader.
newVertexShader :: IO (Shader SFragment)
newVertexShader  = newShader gl_VERTEX_SHADER

-- | DO NOT EXPORT: Create a shader of the specified kind.
newShader :: GLenum -> IO (Shader kind)
newShader kind = Shader `fmap` glCreateShader kind

-- | Free an allocated shader.
freeShader :: Shader kind -> IO ()
freeShader  = glDeleteShader . getShader
