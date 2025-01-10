module Main (main) where

import Server
import System.IO

extractValueFromKey :: [[String]] -> String -> String
extractValueFromKey config_pairs key = last (head (filter (\p -> head p == key) config_pairs))

readConfig :: String -> IO ServerConfig
readConfig filename = do
  file_handle <- openFile filename ReadMode
  contents <- hGetContents file_handle
  let config_rows = lines contents
  let config_pairs = map words config_rows
  let server_port_str = extractValueFromKey config_pairs "server_port"
  let connection_pool_str = extractValueFromKey config_pairs "connection_pool"
  let content_path = extractValueFromKey config_pairs "content_path"
  let default_page = extractValueFromKey config_pairs "default_page"
  let server_port = read server_port_str :: Int
  let connection_pool = read connection_pool_str :: Int
  let result = ServerConfig server_port connection_pool content_path default_page
  return result

main :: IO ()
main = do
  config <- readConfig "config.cfg"
  runServer config
