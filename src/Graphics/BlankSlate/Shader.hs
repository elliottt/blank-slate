{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}

module Graphics.BlankSlate.Shader where

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
loadShader :: Shader kind -> String -> IO ()
loadShader shader program = do
  shaderSource shader program
  compileShader shader
  status <- getShaderiv shader gl_COMPILE_STATUS
  when (status /= gl_TRUE) $ do
    message <- getShaderInfoLog shader
    throwIO (ShaderError message)

-- | Compile a shader from a file.
loadShaderFromFile :: Shader kind -> FilePath -> IO ()
loadShaderFromFile shader path = loadShader shader =<< readFile path


-- Primitive Wrappers ----------------------------------------------------------

-- | Set the program source of a shader.
shaderSource :: Shader kind -> String -> IO ()
shaderSource (Shader n) program =
  withCString program $ \ ptr    ->
  with ptr            $ \ source ->
    glShaderSource n 1 (castPtr source) nullPtr

-- | Compile the shader.
compileShader :: Shader kind -> IO ()
compileShader  = glCompileShader . getShader

-- | Retrieve a status parameter from a shader.
getShaderiv :: Num a => Shader kind -> GLenum -> IO a
getShaderiv (Shader n) param = alloca $ \ statusPtr -> do
  glGetShaderiv n param statusPtr
  fromIntegral `fmap` peek statusPtr

-- | Get the contents of the shader's info log.
getShaderInfoLog :: Shader kind -> IO String
getShaderInfoLog (Shader n) = alloca $ \ lenPtr -> do
  glGetShaderiv n gl_INFO_LOG_LENGTH lenPtr
  glen <- peek lenPtr
  let len = fromIntegral glen
  allocaArray len $ \ buffer -> do
    glGetShaderInfoLog n (fromIntegral glen) nullPtr buffer
    peekCStringLen (castPtr buffer,len)
