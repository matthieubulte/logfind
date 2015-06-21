module LogFindSpec where

import           LogFind
import           Test.Hspec


spec :: Spec
spec = describe "truth" $
  it "should always be True" $
    truth () `shouldBe` True
