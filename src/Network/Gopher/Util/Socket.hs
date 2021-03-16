-- | Internal socket utilities implementing missing
--   features of 'System.Socket' which are yet to be
--   upstreamed.
module Network.Gopher.Util.Socket
  ( gracefulClose
  ) where

import Control.Concurrent.MVar (withMVar)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race)
import Control.Exception.Base (throwIO)
import Control.Monad (void, when)
import Data.Functor ((<&>))
import Foreign.C.Error (Errno (..), getErrno)
import Foreign.C.Types (CInt (..))
import System.Socket (receive, msgNoSignal, SocketException (..), close, Family ())
import System.Socket.Type.Stream (Stream ())
import System.Socket.Protocol.TCP (TCP ())
import System.Socket.Unsafe (Socket (..))

-- Until https://github.com/lpeterse/haskell-socket/pull/67 gets
-- merged, we have to implement shutdown ourselves.
foreign import ccall unsafe "shutdown"
  c_shutdown :: CInt -> CInt -> IO CInt

data ShutdownHow
  -- | Disallow Reading (calls to 'receive' are empty).
  = ShutdownRead
  -- | Disallow Writing (calls to 'send' throw).
  | ShutdownWrite
  -- | Disallow both.
  | ShutdownReadWrite
  deriving (Show, Eq, Ord, Enum)

-- | Shutdown a stream connection (partially).
--   Will send TCP FIN and prompt a client to
--   close the connection.
--
--   Not exposed to prevent future name clash.
shutdown :: Socket a Stream TCP -> ShutdownHow -> IO ()
shutdown (Socket mvar) how = withMVar mvar $ \fd -> do
  res <- c_shutdown (fromIntegral fd)
    $ fromIntegral $ fromEnum how
  when (res /= 0) $ throwIO =<<
    (getErrno <&> \(Errno errno) -> SocketException errno)

-- | Shutdown connection and give client a bit
--   of time to clean up on its end before closing
--   the connection to avoid a broken pipe on the
--   other side.
gracefulClose :: Family f => Socket f Stream TCP -> IO ()
gracefulClose sock = do
  -- send TCP FIN
  shutdown sock ShutdownWrite
  -- wait for some kind of read from the
  -- client (either mempty, meaning TCP FIN,
  -- something else which would mean protocol
  -- violation). Give up after 1s.
  _ <- race (void $ receive sock 16 msgNoSignal) (threadDelay 1000000)
  close sock
