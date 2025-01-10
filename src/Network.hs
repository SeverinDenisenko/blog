module Network
  ( readSocket,
    writeSocket,
    closeConnection,
    openConnection,
    acceptConnection,
  )
where

import Control.Exception (IOException, catch)
import Data.ByteString.Char8 (pack, unpack)
import Network.Socket
import qualified Network.Socket.ByteString as SocketByteString

openConnection :: Int -> Int -> IO Socket
openConnection server_port server_connection_pool = do
  sock <- socket AF_INET Stream 0
  let server_addr = tupleToHostAddress (0, 0, 0, 0)
  let port = fromIntegral server_port
  bind sock (SockAddrInet port server_addr)
  listen sock server_connection_pool
  return sock

acceptConnection :: Socket -> IO Socket
acceptConnection sock = do
  (csock, _) <- accept sock
  return csock

readSocketUnsafe :: Socket -> IO [Char]
readSocketUnsafe csock = do
  string <- SocketByteString.recv csock 4096
  return (unpack string)

readSocket :: Socket -> IO [Char]
readSocket csock = catch (readSocketUnsafe csock) handler
  where
    handler :: IOException -> IO [Char]
    handler ex = do
      print ("Error while reading from socket: " ++ show ex)
      return (show ex)

writeSocketUnsafe :: Socket -> [Char] -> IO Int
writeSocketUnsafe csock string = do
  SocketByteString.send csock (pack string)

writeSocket :: Socket -> [Char] -> IO Int
writeSocket csock string = catch (writeSocketUnsafe csock string) handler
  where
    handler :: IOException -> IO Int
    handler ex = do
      print ("Error while writing to socket: " ++ show ex)
      return 0

closeConnection :: Socket -> IO ()
closeConnection csock = do
  gracefulClose csock 5
