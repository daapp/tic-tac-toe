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



gets :: (TTT.GameState -> b) -> WebM b
gets f = fmap f (ask >>= liftIO . readTVarIO)

modify :: (TTT.GameState -> TTT.GameState) -> WebM ()
modify f = ask >>= liftIO . atomically . flip modifyTVar' f


startApp = do
  sync <- newTVarIO TTT.start
  let runActionToIO m = runReaderT (runWebM m) sync
  scottyT 9999 runActionToIO app

app :: ScottyT Text WebM ()
app = do
  get "/" $ do
    state <- lift $ gets Prelude.id
    -- text $ fromString $ show c
    let ((board, _), winner) = state
        row ix =
            forM_ ix $ \x ->
              case board!x of
                TTT.Nobody ->
                    case winner of
                      Nothing ->
                          H.td $ H.a H.! href (fromString ("/"  ++ show x)) $ " . "
                      _ -> H.td $ fromString " . "
                item ->
                    H.td $ fromString $ show item

    html $ renderHtml
         $ H.html $
              H.body $ do
                      H.table $ do
                         H.tr $ row [1..3]
                         H.tr $ row [4..6]
                         H.tr $ row [7..9]

                      case winner of
                        Nothing -> H.p ""
                        Just w  -> do
                          H.hr
                          H.p $ fromString $ "Winner is " ++ show w
                      H.hr
                      H.a H.! href "/reset" $ "Reset"
  get "/reset" $ do
        lift $ modify $ const TTT.start
        redirect "/"

  get "/:pos" $ do
    pos <- param "pos"
    case reads pos :: [(Int, String)] of
      [(pos', "")]
          | pos' >= 1 && pos' <= 9 -> do
                  lift $ modify $ \st -> TTT.play (fst st) pos'
                  redirect "/"
          | otherwise ->
              redirect "/"

      _ -> redirect "/"

