{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Server
  ( runServer,
    ServerConfig (..),
  )
where

import Control.Exception (IOException, SomeException, catch, try)
import Data.ByteString.Char8 (ByteString, length, pack, unpack)
import Network.Socket
import qualified Network.Socket.ByteString as SocketByteString
import System.FilePath
import System.IO

data ServerConfig = ServerConfig {server_port :: Int, server_connection_pool :: Int, server_content :: [Char], default_page :: [Char]}

runServer :: ServerConfig -> IO ()
runServer server_config = do
  sock <- socket AF_INET Stream 0
  let server_addr = tupleToHostAddress (0, 0, 0, 0)
  let port = fromIntegral (server_port server_config)
  bind sock (SockAddrInet port server_addr)
  listen sock (server_connection_pool server_config)
  catch (serverMainLoop sock server_config) handler
  where
    handler :: SomeException -> IO ()
    handler ex = do
      print ex

serverMainLoop :: Socket -> ServerConfig -> IO ()
serverMainLoop sock server_config = do
  (csock, _) <- accept sock
  handleRequest csock server_config
  serverMainLoop sock server_config

readSocket :: Socket -> IO ByteString
readSocket csock = catch (SocketByteString.recv csock 4096) handler
  where
    handler :: IOException -> IO ByteString
    handler ex = do
      print ex
      return (pack (show ex))

writeSocket :: Socket -> [Char] -> IO Int
writeSocket csock string = catch (SocketByteString.send csock (pack string)) handler
  where
    handler :: IOException -> IO Int
    handler ex = do
      print ex
      return 0

dumpFileContents :: [Char] -> IO [Char]
dumpFileContents name = do
  fileHandle <- openFile name ReadMode
  hGetContents fileHandle

createHttpResponce :: [Char] -> [Char]
createHttpResponce str = "HTTP/1.1 200 OK\r\n" ++ "Content-Length: " ++ show (Prelude.length str) ++ "\r\n" ++ "Content-Type: text/html; charset=utf-8\r\n" ++ "\r\n" ++ str

parceHttpRequest :: [Char] -> ServerConfig -> [Char]
parceHttpRequest header server_config = do
  let header_terms = words header
  let file_name = if Prelude.length header_terms >= 2 then header_terms !! 1 else default_page server_config
  takeDirectory (server_content server_config) </> takeFileName file_name

handleRequest :: Socket -> ServerConfig -> IO ()
handleRequest csock server_config = do
  dat <- readSocket csock
  let filename = parceHttpRequest (unpack dat) server_config
  file_dump_try <- try (dumpFileContents filename) :: IO (Either SomeException [Char])
  case file_dump_try of
    Left ex -> do
      print ex
      close csock
      return ()
    Right file_dump -> do
      let http_responce = createHttpResponce file_dump
      _ <- writeSocket csock http_responce
      if Data.ByteString.Char8.length dat == 0 then close csock else handleRequest csock server_config
