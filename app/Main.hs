module Main where

import           Control.Concurrent (forkOS, threadDelay)
import           Lib
import           Web.Browser        (openBrowser)

port :: Int
port = 9999

-- | Задержка перез запуском браузера в мс
delay :: Int
delay = 3000000


main :: IO ()
main = do
  -- порождаем дочерний процесс, который запустить браузер с url игры через несколько секунд
  forkOS $ do
    threadDelay delay
    let url = "http://localhost:" ++ show port
    _ <- openBrowser url
    putStrLn $ "Open " ++ url
  -- запускаем приложение
  startApp port

