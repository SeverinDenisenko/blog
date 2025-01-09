module Server
  ( serverMainLoop,
  )
where

import Data.ByteString.Char8 (pack, unpack)
import Network.Socket
import qualified Network.Socket.ByteString as ByteString

serverMainLoop :: IO ()
serverMainLoop = do
  sock <- socket AF_INET Stream 0
  let server_addr = tupleToHostAddress (127, 0, 0, 1)
  let server_port = 8500
  let max_queued_connections = 5
  bind sock (SockAddrInet server_port server_addr)
  listen sock max_queued_connections
  (csock, _) <- accept sock
  loop csock

loop :: Socket -> IO ()
loop csock = do
  dat <- ByteString.recv csock 2000
  sent <- ByteString.send csock $ pack "Hi!!"
  print $ unwords ["Received :", unpack dat]
  print $ unwords ["Sent :", show sent]
  loop csock
