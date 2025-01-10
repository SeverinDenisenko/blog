module Main (main) where

import Server

main :: IO ()
main = do
  let server_config = ServerConfig 8000 5 "/home/website/http-haskell/content"
  runServer server_config
