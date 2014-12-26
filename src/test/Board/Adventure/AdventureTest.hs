module Board.Adventure.AdventureTest where

import Test.Hspec

adventureSpec :: Spec
adventureSpec = describe "AdventureTest" $
         it "should be complaning goddammit" $
           True `shouldBe` False

