module Board.Space.SpaceTest where

import Board.Space.Space
import Test.Hspec
import Data.Map
import TalismanErrors.TalismanErrors

spec :: Spec
spec = do
       describe "movement" $ do
         it "moving 1 from SentinelSpace should be Hills1Space and Woods1Space" $ do
           let stringOrSpaces = movementOptions 1 SentinelSpace
           either expectationFailure ( `shouldMatchList` [Hills1Space, Woods1Space]) stringOrSpaces
         it "moving 2 from SentinelSpace should be ChapelSpace and GraveyardSpace" $ do
           let stringOrSpaces = movementOptions 2 SentinelSpace
           either expectationFailure ( `shouldMatchList` [ChapelSpace, GraveyardSpace]) stringOrSpaces
         it "moving 12 from SentinelSpace should be Woods2Space" $ do
           let stringOrSpaces = movementOptions 12 SentinelSpace
           either expectationFailure ( `shouldMatchList` [Woods2Space, Woods2Space]) stringOrSpaces
       describe "lookupSpaces" $ do
         it "should work with empty list of space types to lookup" $ do
           let eitherSpacesOrError = lookupSpaces [] createInitialBoard
           either expectationFailure (`shouldMatchList` []) eitherSpacesOrError
         it "should return an error when space is not present" $ do
           let startingSpace = createStartingSpace SentinelSpace
           let eitherSpacesOrError = lookupSpaces [GraveyardSpace] $
                                       fromList [(SentinelSpace, startingSpace)]
           either (`shouldBe` SpaceTypeNotFound) (expectationFailure . show) eitherSpacesOrError

