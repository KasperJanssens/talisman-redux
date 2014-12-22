module Board.Space.SpaceTest where

import Board.Space.Space
import Test.Hspec

spec :: Spec
spec = describe "SpaceTypes" $
         it "predecessor of first enum should be last" $
           pred Fields1Space `shouldBe` CragsSpace

