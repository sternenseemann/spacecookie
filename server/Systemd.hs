{-# LANGUAGE BlockArguments #-}
module Systemd
  ( systemdSocket
  , notifyReady
  , notifyStopping
  , systemdStoreOrClose
  ) where

import Control.Concurrent.MVar (newMVar, takeMVar)
import Control.Exception.Base
import Network.Gopher (setupGopherSocket, GopherConfig (..))
import qualified Network.Socket as S
import System.Exit
import System.Socket hiding (Error (..))
import System.Socket.Family.Inet6
import System.Socket.Type.Stream
import System.Socket.Protocol.TCP
import System.Socket.Unsafe (Socket (..))
import System.Systemd.Daemon

data SystemdException = NoStream | IncorrectNum
  deriving (Eq, Ord)

instance Show SystemdException where
  show NoStream = "SystemdException: Socket is not of type Stream"
  show IncorrectNum = "SystemdException: Only exactly one Socket is supported"
instance Exception SystemdException

systemdSocket :: GopherConfig -> IO (Socket Inet6 Stream TCP)
systemdSocket cfg = getSock
  where sockConvert :: S.Socket -> IO (Socket Inet6 Stream TCP)
        sockConvert s = S.fdSocket s >>= (fmap Socket . newMVar . fromIntegral)
        getSock = getActivatedSockets >>= \sockets ->
          case sockets of
            Nothing -> setupGopherSocket cfg
            Just [sock] -> do
              t <- S.getSocketType sock
              if t /= S.Stream
                 then throwIO NoStream
                 else sockConvert sock
            Just l -> throwIO IncorrectNum

systemdStoreOrClose :: Socket Inet6 Stream TCP -> IO ()
systemdStoreOrClose s = do
  s' <- kcosConvert s
  res <- storeFd s'
  case res of
    Just () -> return ()
    Nothing -> S.close s'
  where kcosConvert :: Socket Inet6 Stream TCP -> IO S.Socket
        kcosConvert (Socket mvar) = fmap fromIntegral (takeMVar mvar) >>= S.mkSocket
