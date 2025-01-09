module Server
  ( serverMainLoop,
    ServerConfig (..),
  )
where

import Data.ByteString.Char8 (pack, unpack, ByteString)
import Network.Socket
import qualified Network.Socket.ByteString as ByteString
import Control.Exception

data ServerConfig = ServerConfig {server_port :: Int, server_connection_pool :: Int}

serverMainLoop :: ServerConfig -> IO ()
serverMainLoop server_config = do
  sock <- socket AF_INET Stream 0
  let server_addr = tupleToHostAddress (127, 0, 0, 1)
  let port = fromIntegral (server_port server_config)
  bind sock (SockAddrInet port server_addr)
  listen sock (server_connection_pool server_config)
  (csock, _) <- accept sock
  handleRequest csock

readSocket :: Socket -> IO ByteString
readSocket csock = catch (ByteString.recv csock 4096) handler
    where
        handler :: SomeException -> IO ByteString
        handler ex = do
            print ex
            return (pack (show ex))

writeSocket :: Socket -> [Char] -> IO Int
writeSocket csock string = catch (ByteString.send csock (pack string)) handler
    where
        handler :: SomeException -> IO Int
        handler ex = do
            print ex
            return 0

handleRequest :: Socket -> IO ()
handleRequest csock = do
  dat <- readSocket csock
  sent <- writeSocket csock (unlines ["HTTP/1.1 200 OK", "Content-Length: 8", "Accept-Ranges: bytes", "Connection: close", "Hello!!!"])
  print $ unwords ["Received :", unpack dat]
  print $ unwords ["Sent :", show sent]
  close csock
