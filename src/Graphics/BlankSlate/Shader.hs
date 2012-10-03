{-# LANGUAGE EmptyDataDecls #-}

module Graphics.BlankSlate.Shader (
    SVertex
  , SFragment
  , IsShader()

  , Shader()
  , newShader
  , freeShader
  ) where

import System.IO (fixIO)
import Graphics.Rendering.OpenGL.Raw.Core21


data SVertex
data SFragment

class IsShader kind where
  shaderType :: Shader kind -> GLenum

instance IsShader SFragment where
  shaderType _ = gl_FRAGMENT_SHADER

instance IsShader SVertex where
  shaderType _ = gl_VERTEX_SHADER

newtype Shader kind = Shader
  { getShader :: GLuint
  } deriving (Show)

newShader :: IsShader kind => IO (Shader kind)
newShader  = fixIO (\ res -> (Shader `fmap` glCreateShader (shaderType res)))

freeShader :: Shader kind -> IO ()
freeShader  = glDeleteShader . getShader
