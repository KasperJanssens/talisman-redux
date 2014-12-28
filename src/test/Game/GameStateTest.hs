module Game.GameStateTest where

import Test.Hspec
import Game.GameState
import Board.Space.Space
import Character.Character
import Data.Map
import Control.Lens
import Control.Monad.Trans.Either
import Control.Monad.State

spec :: Spec
spec = describe "getOtherPlayersInSamePosition" $
          it "should find the ogre on the same place as the wizard" $ do
            let currentOgreChieftain = ogreChieftain sillyAI & place .~ SentinelSpace
            let currentThief = thief sillyAI
            let currentWizard = wizard sillyAI & place .~ SentinelSpace
            let testPlayers = fromList[(Wizard, currentWizard),
                                (OgreChieftain, currentOgreChieftain),
                                (Thief, currentThief)]
            let gameState = GameState {
              _players = testPlayers,
              _board = undefined,
              _dieRolls = undefined,
              _adventureStack = undefined
            }
            let wizardLens = Lens (at Wizard)
            let eitherResult = evalState
                                  (runEitherT $
                                    getOtherPlayersInSamePosition wizardLens) gameState
            either (expectationFailure . show)
              (`shouldMatchList` [currentOgreChieftain] ) eitherResult
