module Feedback.Test where

import Feedback.Test.OptParse

runFeedbackTest :: IO ()
runFeedbackTest = do
  sets <- getSettings
  print sets
  pure ()
