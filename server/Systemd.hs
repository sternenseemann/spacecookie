{-# LANGUAGE BlockArguments #-}
module Systemd
  ( systemdSocket
  , notifyReady
  , notifyStopping
  , systemdStoreOrClose
  ) where

import Control.Concurrent.MVar (newMVar, takeMVar, mkWeakMVar)
import Control.Exception.Base
import Control.Monad (when, void)
import Foreign.C.Types (CInt (..))
import GHC.Conc (closeFdWith)
import Network.Gopher (setupGopherSocket, GopherConfig (..))
import System.Exit
import System.Posix.Types (Fd (..))
import System.Socket hiding (Error (..))
import System.Socket.Family.Inet6
import System.Socket.Type.Stream
import System.Socket.Protocol.TCP
import System.Socket.Unsafe (Socket (..))
import System.Systemd.Daemon (notifyReady, notifyStopping)
import System.Systemd.Daemon.Fd (storeFd, getActivatedSockets)

foreign import ccall unsafe "close"
  c_close :: CInt -> IO CInt

-- TODO Check Socket type, ...
data SystemdException = IncorrectNum | InvalidFd
  deriving (Eq, Ord)

instance Show SystemdException where
  show IncorrectNum = "SystemdException: Only exactly one Socket is supported"
  show InvalidFd    = "SystemdException: Invalid File Descriptor received"
instance Exception SystemdException

systemdSocket :: GopherConfig -> IO (Socket Inet6 Stream TCP)
systemdSocket cfg = getActivatedSockets >>= \sockets ->
  case sockets of
    Nothing -> setupGopherSocket cfg
    Just [fd] -> toSocket fd
    Just _ -> throwIO IncorrectNum
  where toSocket :: Fd -> IO (Socket Inet6 Stream TCP)
        toSocket fd = do
          when (fd < 0) $ throwIO InvalidFd
          mfd <- newMVar (fromIntegral fd)
          let s = Socket mfd
          _ <- mkWeakMVar mfd (close s)
          pure s

systemdStoreOrClose :: Socket Inet6 Stream TCP -> IO ()
systemdStoreOrClose s = do
  fd <- toFd s
  res <- storeFd fd
  case res of
    Just () -> return ()
    Nothing -> closeFdWith (void . c_close . fromIntegral) fd
  where toFd :: Socket Inet6 Stream TCP -> IO Fd
        toFd (Socket mvar) = fmap (Fd . fromIntegral) (takeMVar mvar)
