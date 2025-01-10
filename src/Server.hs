{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Server
  ( runServer,
    ServerConfig (..),
  )
where

import Control.Exception (Exception, IOException, SomeException, catch, try)
import Control.Exception.Base (throw)
import Data.ByteString.Char8 (ByteString, length, pack, unpack)
import Network.Socket
import qualified Network.Socket.ByteString as SocketByteString
import System.FilePath
import System.IO

data ServerConfig = ServerConfig
  { server_port :: Int,
    server_connection_pool :: Int,
    server_content :: [Char],
    default_page :: [Char]
  }

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

closeConnection :: Socket -> IO ()
closeConnection csock = close csock

dumpFileContents :: [Char] -> IO [Char]
dumpFileContents name = do
  fileHandle <- openFile name ReadMode
  hGetContents fileHandle

createHttpResponse :: [Char] -> [Char]
createHttpResponse str = "HTTP/1.1 200 OK\r\n" ++ "Content-Length: " ++ show (Prelude.length str) ++ "\r\n" ++ "Content-Type: text/html; charset=utf-8\r\n" ++ "\r\n" ++ str

data HTTPException = HTTPException deriving (Show)

instance Exception HTTPException

data HTTPRequest = HTTPRequest
  { http_method :: [Char],
    http_path :: [Char],
    http_request_protocol_version :: [Char]
  }

data HTTPResponse = HTTPResponse
  { http_response_protocol_version :: [Char],
    http_response_code :: [Char],
    http_response_status :: [Char],
    http_response_content_length :: Int,
    http_response_content_type :: [Char]
  }

parceHttpRequest :: [Char] -> HTTPRequest
parceHttpRequest request = do
  let request_tokens = words request
  if Prelude.length request_tokens < 3
    then throw HTTPException
    else do
      let method = request_tokens !! 0
      let path = request_tokens !! 1
      let protocol_version = request_tokens !! 2
      HTTPRequest method path protocol_version

getResponseFilePath :: [Char] -> ServerConfig -> [Char]
getResponseFilePath path server_config = takeDirectory (server_content server_config) </> takeFileName path

createGETResponse :: Socket -> ServerConfig -> HTTPRequest -> IO ()
createGETResponse csock server_config request = do
  let response_file = getResponseFilePath (http_path request) server_config
  file_dump_try <- try (dumpFileContents response_file) :: IO (Either SomeException [Char])
  case file_dump_try of
    Left ex -> do
      print ex
      closeConnection csock
      return ()
    Right file_dump -> do
      let http_response = createHttpResponse file_dump
      _ <- writeSocket csock http_response
      handleRequest csock server_config

createResponse :: Socket -> ServerConfig -> HTTPRequest -> IO ()
createResponse csock server_config request
  | http_method request == "GET" = createGETResponse csock server_config request
  | http_method request == "POST" = closeConnection csock
  | http_method request == "PUT" = closeConnection csock
  | http_method request == "DEL" = closeConnection csock
  | otherwise = closeConnection csock

handleRequest :: Socket -> ServerConfig -> IO ()
handleRequest csock server_config = do
  dat <- readSocket csock
  if Data.ByteString.Char8.length dat == 0
    then close csock
    else do
      let request = parceHttpRequest (unpack dat)
      createResponse csock server_config request
