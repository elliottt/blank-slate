{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}

module Graphics.BlankSlate.Shader (
    SVertex
  , SFragment

  , Shader(..), ShaderError(..)
  , newVertexShader, newFragmentShader
  , freeShader
  , compileShader
  , compileShaderFromFile
  ) where

import Control.Exception (Exception(..),throwIO)
import Control.Monad (when)
import Data.Typeable (Typeable)
import Foreign (nullPtr,castPtr,alloca,allocaArray,peek,with)
import Foreign.C.String (withCString,peekCStringLen)
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

data ShaderError = ShaderError String
    deriving (Eq,Show,Typeable)

instance Exception ShaderError

-- | Compile a shader from a string.  Throws a @ShaderError@ if there was a
-- problem compiling the shader.
compileShader :: String -> Shader kind -> IO ()
compileShader program shader@(Shader n) =
  withCString program $ \ ptr    ->
  with ptr            $ \ source -> do
    glShaderSource n 1 (castPtr source) nullPtr
    glCompileShader n
    status <- getShaderParam shader gl_COMPILE_STATUS
    when (fromIntegral status /= gl_TRUE) $ do
      message <- getShaderInfoLog shader
      throwIO (ShaderError message)

-- | Compile a shader from a file.
compileShaderFromFile :: FilePath -> Shader kind -> IO ()
compileShaderFromFile path shader = do
  source <- readFile path
  compileShader source shader

-- | Retrieve a status parameter from a shader.
getShaderParam :: Shader kind -> GLenum -> IO GLint
getShaderParam (Shader n) param = alloca $ \ statusPtr -> do
  glGetShaderiv n param statusPtr
  peek statusPtr

-- | Get the contents of the shader's info log.
getShaderInfoLog :: Shader kind -> IO String
getShaderInfoLog (Shader n) = alloca $ \ lenPtr -> do
  glGetShaderiv n gl_INFO_LOG_LENGTH lenPtr
  glen <- peek lenPtr
  let len = fromIntegral glen
  allocaArray len $ \ buffer -> do
    glGetShaderInfoLog n (fromIntegral glen) nullPtr buffer
    peekCStringLen (castPtr buffer,len)
