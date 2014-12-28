module DungeonMaster.MovementTest where

import Test.Hspec
import Character.Character
import Game.GameState
import Data.Map
import Board.Space.Space
import DungeonMaster.Movement
import Control.Lens
import Control.Monad.State
import Control.Monad.Trans.Either

spec :: Spec
spec = describe "Movement Phase" $
         it "should move a character correctly, and update the dieRolls" $ do
            let testPlayers = fromList[(Wizard, wizard sillyAI),
                                (OgreChieftain, ogreChieftain sillyAI),
                                (Thief, thief sillyAI)]
            let gameState = GameState {
             _players = testPlayers,
             _board = createInitialBoard,
             _dieRolls = [1],
             _adventureStack = undefined
            }
            actualGameState <- execStateT (runEitherT
                                      $ movePhase $ Lens $ at Wizard) gameState
            actualGameState ^. dieRolls `shouldMatchList` []
            let maybeWizard = actualGameState ^. players . at Wizard
            maybe (expectationFailure "not found") (\player -> (player ^. place) `shouldBe` Woods1Space) maybeWizard