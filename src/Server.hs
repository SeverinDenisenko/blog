{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Server
  ( runServer,
    ServerConfig (..),
  )
where

import Control.Exception (Exception, SomeException, catch, try)
import Control.Exception.Base (throw)
import Files
import Network
import Network.Socket
import System.FilePath

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
  catch (handleRequest csock server_config >> serverMainLoop sock server_config) handler
  where
    handler :: SomeException -> IO ()
    handler ex = do
      print ex
      serverMainLoop sock server_config

data HTTPException = HTTPException deriving (Show)

instance Exception HTTPException

data HTTPRequest = HTTPRequest
  { http_method :: [Char],
    http_path :: [Char],
    http_request_protocol_version :: [Char]
  }

data HTTPResponse = HTTPResponse
  { http_response_protocol_version :: [Char],
    http_response_code :: Int,
    http_response_status :: [Char],
    http_response_content_length :: Int,
    http_response_content_type :: [Char],
    http_response_content :: [Char]
  }

data HTTPErrorResponse = HTTPErrorResponse
  { http_error_protocol_version :: [Char],
    http_error_code :: Int,
    http_error_status :: [Char],
    http_error_location :: [Char]
  }

creteDataFromHTTPResponse :: HTTPResponse -> [Char]
creteDataFromHTTPResponse response = do
  let first_line = http_response_protocol_version response ++ " " ++ show (http_response_code response) ++ " " ++ http_response_status response
  let second_line = "Content-Length: " ++ show (http_response_content_length response)
  let third_line = "Content-Type: " ++ http_response_content_type response
  first_line ++ "\r\n" ++ second_line ++ "\r\n" ++ third_line ++ "\r\n\r\n" ++ http_response_content response

creteDataFromHTTPErrorResponse :: HTTPErrorResponse -> [Char]
creteDataFromHTTPErrorResponse response
  | http_error_code response == 308 = do
      let first_line = http_error_protocol_version response ++ " " ++ show (http_error_code response) ++ " " ++ http_error_status response
      let second_line = "Location: " ++ http_error_location response
      first_line ++ "\r\n" ++ second_line ++ "\r\n\r\n"
  | otherwise = do
      let first_line = http_error_protocol_version response ++ " " ++ show (http_error_code response) ++ " " ++ http_error_status response
      first_line ++ "\r\n\r\n"

parceHttpRequest :: [Char] -> HTTPRequest
parceHttpRequest request = do
  let request_tokens = words request
  if length request_tokens < 3
    then throw HTTPException
    else do
      let method = request_tokens !! 0
      let path = request_tokens !! 1
      let protocol_version = request_tokens !! 2
      HTTPRequest method path protocol_version

getResponseFilePath :: [Char] -> ServerConfig -> [Char]
getResponseFilePath path server_config = takeDirectory (server_content server_config) </> takeFileName path

extentionToContentType :: [Char] -> [Char]
extentionToContentType ext
  | ext == ".html" = "text/html; charset=utf-8"
  | ext == ".css" = "text/css"
  | ext == ".js" = "text/javascript; charset=utf-8"
  | ext == ".ico" = "image/x-icon"
  | ext == ".png" = "image/png"
  | otherwise = "text"

createGETResponse :: Socket -> ServerConfig -> HTTPRequest -> IO ()
createGETResponse csock server_config request = do
  let response_file = getResponseFilePath (http_path request) server_config
  let extention = takeExtension response_file
  file_dump_try <- try (dumpFileContents response_file) :: IO (Either SomeException [Char])
  case file_dump_try of
    Left _ -> do
      let response = HTTPErrorResponse (http_request_protocol_version request) 308 "Moved Permanently" (default_page server_config)
      let response_str = creteDataFromHTTPErrorResponse response
      _ <- writeSocket csock response_str
      closeConnection csock
    Right file_dump -> do
      let content_type = extentionToContentType extention
      let content_size = length file_dump
      let response = HTTPResponse (http_request_protocol_version request) 200 "OK" content_size content_type file_dump
      let response_str = creteDataFromHTTPResponse response
      _ <- writeSocket csock response_str
      closeConnection csock

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
  if length dat == 0
    then
      closeConnection csock
    else do
      let request = parceHttpRequest dat
      createResponse csock server_config request
