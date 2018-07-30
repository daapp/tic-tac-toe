module Main where

import           Control.Concurrent (forkOS, threadDelay)
import           Lib
import           Web.Browser        (openBrowser)

port :: Int
port = 9999

main :: IO ()
main = do
  forkOS $ do
    threadDelay 3000000
    r <- openBrowser $ "http://localhost:" ++ (show port)
    print r
  startApp port

