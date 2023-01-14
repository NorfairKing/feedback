module FeedbackSpec (spec) where

import System.Process.Typed
import Test.Syd

spec :: Spec
spec = sequential $ do
  it "can show help text" $ do
    let cp = setStdout nullStream $ proc "feedback" ["--help"]
    runProcess_ cp :: IO ()
  pure ()
