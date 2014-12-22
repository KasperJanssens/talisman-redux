module Board.Adventure.AdventureTest where

import Test.Hspec

spec :: Spec
spec = describe "AdventureTest" $
         it "should be complaning goddammit" $
           True `shouldBe` False

