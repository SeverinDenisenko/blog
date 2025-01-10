module Main (main) where

import Server

main :: IO ()
main = do
  let server_config = ServerConfig 8000 5 "/home/website/http-haskell/content/" "index.html"
  runServer server_config
