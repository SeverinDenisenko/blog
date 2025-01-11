{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Server
  ( runServer,
    ServerConfig (..),
  )
where

import Control.Concurrent (forkIO)
import Control.Exception (Exception, SomeException, catch, try)
import Control.Exception.Base (throw)
import Files
import Network
import Network.Socket
import System.FilePath

data ServerConfig = ServerConfig
  { server_port :: Int,
    server_connection_pool :: Int,
    server_content :: String,
    default_page :: String
  }

runServer :: ServerConfig -> IO ()
runServer server_config = do
  sock <- openConnection (server_port server_config) (server_connection_pool server_config)
  serverMainLoop sock server_config

serverMainLoop :: Socket -> ServerConfig -> IO ()
serverMainLoop sock server_config = do
  csock <- acceptConnection sock
  _ <- forkIO (handleRequest csock server_config)
  serverMainLoop sock server_config

data HTTPException = HTTPException deriving (Show)

instance Exception HTTPException

data HTTPRequest = HTTPRequest
  { http_method :: String,
    http_path :: String,
    http_request_protocol_version :: String
  }

data HTTPResponse = HTTPResponse
  { http_response_protocol_version :: String,
    http_response_code :: Int,
    http_response_status :: String,
    http_response_content_length :: Int,
    http_response_content_type :: String,
    http_response_content :: String
  }

data HTTPErrorResponse = HTTPErrorResponse
  { http_error_protocol_version :: String,
    http_error_code :: Int,
    http_error_status :: String
  }

data HTTPRedirectResponse = HTTPRedirectResponse
  { http_redirect_protocol_version :: String,
    http_redirect_code :: Int,
    http_redirect_status :: String,
    http_redirect_location :: String
  }

creteDataFromHTTPResponse :: HTTPResponse -> String
creteDataFromHTTPResponse response = do
  let first_line = http_response_protocol_version response ++ " " ++ show (http_response_code response) ++ " " ++ http_response_status response
  let second_line = "Content-Length: " ++ show (http_response_content_length response)
  let third_line = "Content-Type: " ++ http_response_content_type response
  first_line ++ "\r\n" ++ second_line ++ "\r\n" ++ third_line ++ "\r\n\r\n" ++ http_response_content response

creteDataFromHTTPErrorResponse :: HTTPErrorResponse -> String
creteDataFromHTTPErrorResponse response = do
  let first_line = http_error_protocol_version response ++ " " ++ show (http_error_code response) ++ " " ++ http_error_status response
  first_line ++ "\r\n\r\n"

creteDataFromHTTPRedirectResponse :: HTTPRedirectResponse -> String
creteDataFromHTTPRedirectResponse response = do
  let first_line = http_redirect_protocol_version response ++ " " ++ show (http_redirect_code response) ++ " " ++ http_redirect_status response
  let second_line = "Location: " ++ http_redirect_location response
  first_line ++ "\r\n" ++ second_line ++ "\r\n\r\n"

parceHttpRequest :: String -> HTTPRequest
parceHttpRequest request = do
  let request_tokens = words request
  if length request_tokens < 3
    then throw HTTPException
    else do
      let method = request_tokens !! 0
      let path = request_tokens !! 1
      let protocol_version = request_tokens !! 2
      HTTPRequest method path protocol_version

getResponseFilePath :: String -> ServerConfig -> String
getResponseFilePath path server_config = takeDirectory (server_content server_config) </> takeFileName path

extentionToContentType :: String -> String
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
  file_dump_try <- try (dumpFileContents response_file) :: IO (Either SomeException String)
  case file_dump_try of
    Left _ -> do
      if extention == ".html"
        then do
          print ("Can't send file: " ++ response_file)
          let response = HTTPRedirectResponse "HTTP/1.1" 308 "Moved Permanently" (default_page server_config)
          let response_str = creteDataFromHTTPRedirectResponse response
          _ <- writeSocket csock response_str
          closeConnection csock
        else closeConnection csock
    Right file_dump -> do
      let content_type = extentionToContentType extention
      let content_size = length file_dump
      let response = HTTPResponse "HTTP/1.1" 200 "OK" content_size content_type file_dump
      let response_str = creteDataFromHTTPResponse response
      _ <- writeSocket csock response_str
      closeConnection csock

createNotAllowedResponse :: Socket -> HTTPRequest -> IO ()
createNotAllowedResponse csock request = do
  let response = HTTPErrorResponse "HTTP/1.1" 405 "Method Not Allowed"
  let response_str = creteDataFromHTTPErrorResponse response
  _ <- writeSocket csock response_str
  closeConnection csock

createResponse :: Socket -> ServerConfig -> HTTPRequest -> IO ()
createResponse csock server_config request
  | http_method request == "GET" = createGETResponse csock server_config request
  | http_method request == "POST" = createNotAllowedResponse csock request
  | http_method request == "PUT" = createNotAllowedResponse csock request
  | http_method request == "DEL" = createNotAllowedResponse csock request
  | http_method request == "TRACE" = createNotAllowedResponse csock request
  | http_method request == "HEAD" = createNotAllowedResponse csock request
  | http_method request == "CONNECT" = createNotAllowedResponse csock request
  | http_method request == "OPTIONS" = createNotAllowedResponse csock request
  | http_method request == "PATCH" = createNotAllowedResponse csock request
  | otherwise = closeConnection csock

handleRequest :: Socket -> ServerConfig -> IO ()
handleRequest csock server_config = do
  dat <- readSocket csock
  if null dat
    then do
      closeConnection csock
      print "Session was unexpectedly dropped"
    else do
      let request = parceHttpRequest dat
      catch (createResponse csock server_config request) handle
  where
    handle :: HTTPException -> IO ()
    handle ex = do
      closeConnection csock
      print ("Error processing responce: " ++ show ex)
