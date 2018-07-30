{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeSynonymInstances       #-}
module Lib where

import           Control.Applicative
import           Control.Concurrent.STM
import           Control.Monad.Reader
import           Data.Array
import           Data.Default.Class
import           Data.String                   (fromString)
import           Data.Text.Lazy                (Text)
import           Prelude
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Text.Blaze.Html5              as H
import           Text.Blaze.Html5.Attributes
import qualified TicTacToe                     as TTT
import           Web.Scotty.Trans

instance Default TTT.GameState where
  def = TTT.start

newtype WebM a = WebM { runWebM :: ReaderT (TVar TTT.GameState) IO a }
  deriving (Applicative, Functor, Monad, MonadIO, MonadReader (TVar TTT.GameState))


-- | Монадный трансформер. Применяет функцию f к состоянию. Возвращает результат выполнения f обёрнутый в WebM.
-- | gets предназначен для считывания состояния.
gets :: (TTT.GameState -> b) -> WebM b
gets f = fmap f (ask >>= liftIO . readTVarIO)

-- | Монадный трансформер. Применяет функцию f к состоянию. Результат выполнения функции f будет являться новым состоянием.
modify :: (TTT.GameState -> TTT.GameState) -> WebM ()
modify f = ask >>= liftIO . atomically . flip modifyTVar' f

-- | Запускает web сервер на указанном порту.
startApp port = do
  -- создаём TVar, хранящую состояние игры
  sync <- newTVarIO TTT.start
  -- создаём трансформер
  let runActionToIO m = runReaderT (runWebM m) sync
  -- запускаем web сервер
  scottyT port runActionToIO app

app :: ScottyT Text WebM ()
app = do
  -- показываем игровое поле
  get "/" $ do
    state <- lift $ gets Prelude.id
    let ((board, _), winner) = state
        -- функция форматирует строку таблицы
        row ix =
            forM_ ix $ \x ->
              case board!x of
                -- пустая ячейка доски
                TTT.Nobody ->
                    -- если есть победитель, то играна завершена и ссылки на пустые ячейки доски не нужны
                    case winner of
                      Nothing ->
                          H.td $ H.a H.! href (fromString ("/"  ++ show x)) $ " . "
                      _ -> H.td $ fromString " . "
                -- занятия ячейка доски
                item ->
                    H.td $ fromString $ show item

    html $ renderHtml
         $ H.html $
              H.body $ do
                      H.table $ do
                         H.tr $ row [1..3]
                         H.tr $ row [4..6]
                         H.tr $ row [7..9]

                      -- если есть победитель - указываем его
                      case winner of
                        Nothing -> H.p ""
                        Just w  -> do
                          H.hr
                          H.p $ fromString $ "Winner is " ++ show w
                      H.hr
                      H.a H.! href "/reset" $ "Reset"
  -- сброс игры в первоначальное состояние
  get "/reset" $ do
        -- сбрасываем состояние игры в начальное
        lift $ modify $ const TTT.start
        -- показываем игровое поле
        redirect "/"

  -- указание клетки с новым ходом для X
  get "/:pos" $ do
    pos <- param "pos"
    case reads pos :: [(Int, String)] of
      -- если в качестве параметра задано только целое число - делаем ход
      [(pos', "")]
          -- если pos в диапазоне 1..9 - то ход считаем правильным
          | pos' >= 1 && pos' <= 9 -> do
                  -- изменяем игровое состояние
                  lift $ modify $ \st -> TTT.play (fst st) pos'
                  -- показываем игровое поле
                  redirect "/"
          -- указан неправильный индекс
          | otherwise ->
              -- показываем иговое поле
              redirect "/"
      -- иначе - считаем, что произошла ошибка и показываем игровое поле
      _ -> redirect "/"

