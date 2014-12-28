module DungeonMaster.FightTest where

import Test.Hspec
import Character.Character
import Game.GameState
import Data.Map
import Board.Space.Space
import DungeonMaster.Fight
import Control.Lens
import Control.Monad.State
import Control.Monad.Trans.Either

spec :: Spec
spec = do
       describe "determineAttackStrength" $
         it "should correctly determine the attack strength of the wizard" $ do
           let currentWizard = wizard sillyAI
           let gameState = GameState {
             _players = fromList [(Wizard, currentWizard)]
             , _dieRolls = [4]
             ,_adventureStack = undefined
             ,_board = undefined
           }
           let (actualStrengthOrError, actualGameState) = runState (runEitherT $
                                              determineAttackStrength (Lens $ at Wizard)) gameState
           either (expectationFailure . show) (`shouldBe` currentWizard ^. character . strength + 4) actualStrengthOrError
           actualGameState ^. dieRolls `shouldMatchList` []
       describe "loseFight" $
         it "should correctly subtract life of the wizard" $ do
            let currentWizard = wizard sillyAI
            let gameState = GameState {
              _players = fromList [(Wizard, currentWizard)]
              , _dieRolls = undefined
              ,_adventureStack = undefined
              ,_board = undefined
            }
            let actualGameState = execState (runEitherT $
                                       loseFight (Lens $ at Wizard)) gameState
            maybe (expectationFailure "Not found")
              (\actualWizard -> actualWizard ^. character. life `shouldBe` 3)
                $ actualGameState ^. players . at Wizard
       describe "fight" $ do
         it "should make the defender lose" $ do
           let currentWizard = wizard sillyAI
               currentThief = thief sillyAI
               gameState = GameState {
                _players = fromList [(Wizard, currentWizard), (Thief, currentThief)]
                , _dieRolls =[3,1]
                , _adventureStack = undefined
                , _board = undefined
               }
           actualState <- execStateT (runEitherT (fight (Lens $ at Wizard) (Lens $ at Thief))) gameState
           maybe (expectationFailure "not found")
            (\actualWizard -> actualWizard ^. character . life `shouldBe` 4) $
             actualState ^. players . at Wizard
           maybe (expectationFailure "not found")
            (\actualThief -> actualThief ^. character . life `shouldBe` 3) $
             actualState ^. players . at Thief
         it "should make the attacker lose" $ do
           let currentWizard = wizard sillyAI
               currentThief = thief sillyAI
               gameState = GameState {
                _players = fromList [(Wizard, currentWizard), (Thief, currentThief)]
                , _dieRolls =[1,1]
                , _adventureStack = undefined
                , _board = undefined
               }
           actualState <- execStateT (runEitherT (fight (Lens $ at Wizard) (Lens $ at Thief))) gameState
           maybe (expectationFailure "not found")
            (\actualWizard -> actualWizard ^. character . life `shouldBe` 3) $
             actualState ^. players . at Wizard
           maybe (expectationFailure "not found")
            (\actualThief -> actualThief ^. character . life `shouldBe` 4) $
             actualState ^. players . at Thief
         it "should be a draw" $ do
           let currentWizard = wizard sillyAI
               currentThief = thief sillyAI
               gameState = GameState {
                _players = fromList [(Wizard, currentWizard), (Thief, currentThief)]
                , _dieRolls =[2,1]
                , _adventureStack = undefined
                , _board = undefined
               }
           actualState <- execStateT (runEitherT (fight (Lens $ at Wizard) (Lens $ at Thief))) gameState
           maybe (expectationFailure "not found")
            (\actualWizard -> actualWizard ^. character . life `shouldBe` 4) $
             actualState ^. players . at Wizard
           maybe (expectationFailure "not found")
            (\actualThief -> actualThief ^. character . life `shouldBe` 4) $
             actualState ^. players . at Thief

