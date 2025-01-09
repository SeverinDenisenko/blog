module Server
  ( runServer,
    ServerConfig (..),
  )
where

import Control.Exception
import Data.ByteString.Char8 (ByteString, length, pack, unpack)
import Network.Socket
import qualified Network.Socket.ByteString as SocketByteString

data ServerConfig = ServerConfig {server_port :: Int, server_connection_pool :: Int}

runServer :: ServerConfig -> IO ()
runServer server_config = do
  sock <- socket AF_INET Stream 0
  let server_addr = tupleToHostAddress (127, 0, 0, 1)
  let port = fromIntegral (server_port server_config)
  bind sock (SockAddrInet port server_addr)
  listen sock (server_connection_pool server_config)
  serverMainLoop sock

serverMainLoop :: Socket -> IO ()
serverMainLoop sock = do
  (csock, _) <- accept sock
  handleRequest csock
  serverMainLoop sock

readSocket :: Socket -> IO ByteString
readSocket csock = catch (SocketByteString.recv csock 4096) handler
  where
    handler :: SomeException -> IO ByteString
    handler ex = do
      print ex
      return (pack (show ex))

writeSocket :: Socket -> [Char] -> IO Int
writeSocket csock string = catch (SocketByteString.send csock (pack string)) handler
  where
    handler :: SomeException -> IO Int
    handler ex = do
      print ex
      return 0

handleRequest :: Socket -> IO ()
handleRequest csock = do
  dat <- readSocket csock
  sent <- writeSocket csock (unlines ["HTTP/1.1 200 OK", "Content-Length: 12", "Content-Type: text/plain; charset=utf-8", "", "Hello World!", ""])
  print $ unwords ["Received :", unpack dat]
  print $ unwords ["Sent :", show sent]
  if Data.ByteString.Char8.length dat == 0 then close csock else handleRequest csock
