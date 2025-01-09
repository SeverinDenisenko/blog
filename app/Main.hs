module Main (main) where

import Server

main :: IO ()
main = do
  let server_config = ServerConfig 8000 5 "/Users/severin/Documents/GitHub/http-haskel/content"
  runServer server_config
