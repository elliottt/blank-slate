module Graphics.BlankSlate.Prim.Buffer where

import Foreign (allocaArray,withArrayLen,peekArray)
import Graphics.Rendering.OpenGL.Raw.Core31


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
