{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.Spacecookie.Systemd
  ( systemdSocket
  , notifyReady
  , notifyStopping
  , systemdStoreOrClose
  , SystemdException (..)
  ) where

import Control.Concurrent.MVar (newMVar, swapMVar, mkWeakMVar)
import Control.Exception.Base
import Control.Monad (when)
import Data.Maybe (fromMaybe)
import Foreign.C.Types (CInt (..))
import GHC.Conc (closeFdWith)
import Network.Gopher
import System.IO.Error (mkIOError, userErrorType)
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

-- | Close a 'Fd' using close(1). Throws an 'IOException' on error.
closeFd :: Fd -> IO ()
closeFd fd = do
  res <- c_close $ fromIntegral fd
  when (res /= 0) $ throwIO
    $ mkIOError userErrorType "Could not close File Descriptor" Nothing Nothing

-- | Irreversibly convert a 'Socket' into an 'Fd'.
--   Invalidates the socket and returns the file descriptor
--   contained within it.
toFd :: Socket a b c -> IO Fd
toFd (Socket mvar) = fmap (Fd . fromIntegral) (swapMVar mvar (-1))
-- putting an invalid file descriptor into the 'MVar' makes
-- the 'Socket' appear to System.Socket as if it were closed

-- | Create an 'Socket' from an 'Fd'. This action is unsafe
--   since the type of the socket is not checked meaning that
--   whatever type the resulting 'Socket' has is not guaranteed
--   to be the same as its type indicates. Thus, this function
--   needs to be used with care so the safety guarantees of
--   'Socket' are not violated.
--
--   Throws an 'IOException' if the 'Fd' is invalid.
fromFd :: Fd -> IO (Socket a b c)
fromFd fd = do
  -- TODO Validate socket type
  when (fd < 0) $ throwIO
    $ mkIOError userErrorType "Invalid File Descriptor" Nothing Nothing
  mfd <- newMVar (fromIntegral fd)
  let s = Socket mfd
  _ <- mkWeakMVar mfd (close s)
  pure s

data SystemdException
  = IncorrectNum
  deriving (Eq, Ord)

instance Exception SystemdException
instance Show SystemdException where
  show IncorrectNum = "SystemdException: Only exactly one Socket is supported"

systemdSocket :: GopherConfig -> IO (Socket Inet6 Stream TCP)
systemdSocket cfg = getActivatedSockets >>= \sockets ->
  case sockets of
    Nothing -> setupGopherSocket cfg
    Just [fd] -> do
      listenWarning
      fromFd fd
    Just _ -> throwIO IncorrectNum
  where listenWarning = fromMaybe (pure ()) $ do
          logAction <- cLogHandler cfg
          addr <- cListenAddr cfg
          pure . logAction GopherLogLevelWarn
            $ mconcat
            [ "Listen address ", toGopherLogStr addr
            , " specified, but started with systemd socket."
            , " Using systemd, listen address may differ." ]

systemdStoreOrClose :: Socket Inet6 Stream TCP -> IO ()
systemdStoreOrClose s = do
  fd <- toFd s
  res <- storeFd fd
  case res of
    Just () -> return ()
    Nothing -> closeFdWith closeFd fd
