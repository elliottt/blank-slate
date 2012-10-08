module Graphics.BlankSlate.Mesh (
    module Graphics.BlankSlate.Mesh

    -- * Buffers
  , gl_ARRAY_BUFFER
  , gl_ELEMENT_ARRAY_BUFFER
  , gl_PIXEL_PACK_BUFFER
  , gl_PIXEL_UNPACK_BUFFER

    -- * Usage Hints
  , gl_STATIC_DRAW, gl_STREAM_DRAW, gl_DYNAMIC_DRAW
  , gl_STATIC_READ, gl_STREAM_READ, gl_DYNAMIC_READ
  , gl_STATIC_COPY, gl_STREAM_COPY, gl_DYNAMIC_COPY
  ) where

import Foreign (allocaArray,withArrayLen,peekArray,Storable,castPtr)
import Data.Array.Storable (StorableArray,withStorableArray,getBounds)
import Graphics.Rendering.OpenGL.Raw.Core21


-- Data Buffers ----------------------------------------------------------------

newtype Buffer = Buffer { getBuffer :: GLuint }
    deriving (Show)

-- | Generate a single buffer.
genBuffer :: IO Buffer
genBuffer  = do
  bufs <- genBuffers 1
  case bufs of
    [buf] -> return buf
    _     -> fail "genBuffer: Unexpected number of results"

-- | Gen many buffers all at once.
genBuffers :: Int -> IO [Buffer]
genBuffers n = allocaArray n $ \ ptr -> do
  glGenBuffers (fromIntegral n) ptr
  map Buffer `fmap` peekArray n ptr

deleteBuffer :: Buffer -> IO ()
deleteBuffer buf = deleteBuffers [buf]

-- | Free a list of buffer objects.
deleteBuffers :: [Buffer] -> IO ()
deleteBuffers bufs = withArrayLen (map getBuffer bufs) $ \ len ptr ->
  glDeleteBuffers (fromIntegral len) ptr

bindBuffer :: GLenum -> Buffer -> IO ()
bindBuffer name = glBindBuffer name . getBuffer

unbindBuffer :: GLenum -> IO ()
unbindBuffer name = bindBuffer name (Buffer 0)


-- Meshes ----------------------------------------------------------------------

-- | Get the size of a @StorableArray@, indexed by @Int@s, assuming that it
-- starts at 0, and goes to n - 1.
storableArraySize :: Storable a => StorableArray Int a -> IO GLsizeiptr
storableArraySize a = prjSize `fmap` getBounds a
  where
  prjSize (_,n) = fromIntegral (n+1)

-- | Set the buffer data for an allocated buffer, with the content of a mesh.
bufferData :: Storable a => GLenum -> StorableArray Int a -> GLenum -> IO ()
bufferData target a usage = do
  len <- storableArraySize a
  withStorableArray a $ \ ptr ->
    glBufferData target len (castPtr ptr) usage
