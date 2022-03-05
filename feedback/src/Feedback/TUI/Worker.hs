module Feedback.TUI.Worker where

import Brick.BChan
import Control.Monad
import Control.Monad.IO.Class
import Feedback.TUI.Env

tuiWorker :: BChan Request -> BChan Response -> W ()
tuiWorker reqChan respChan = forever $ do
  req <- liftIO $ readBChan reqChan
  resp <- case req of
    ReceiveEvent event -> pure $ ReceivedEvent event
  liftIO $ writeBChan respChan resp
