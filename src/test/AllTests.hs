module Main where
import qualified Board.Adventure.AdventureTest as AdventureTest
import qualified Board.Space.SpaceTest as SpaceTest
import qualified Game.GameStateTest as GameStateTest
import qualified DungeonMaster.MovementTest as MovementTest
import qualified DungeonMaster.FightTest as FightTest

import Test.Hspec

main :: IO()
main = hspec $
 describe "Unit tests" $ do
  AdventureTest.adventureSpec
  SpaceTest.spec
  GameStateTest.spec
  MovementTest.spec
  FightTest.spec

