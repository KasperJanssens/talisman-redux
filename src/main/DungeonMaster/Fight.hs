{-# LANGUAGE FlexibleContexts #-}

module DungeonMaster.Fight where

import Game.GameState
import Control.Lens
import Character.Character
import Control.Monad.State
import Control.Monad.Trans.Either
import TalismanErrors.TalismanErrors

determineAttackStrength :: (MonadState GameState m) => ReifiedLens' Players (Maybe Player) ->
 EitherT TalismanErrors m Int
determineAttackStrength playerLens = do
  currentDieRolls <- gets (^. dieRolls)
  currentPlayers <- gets (^. players)
  modify ( & dieRolls %~ tail )
  currentPlayer <- maybe (left PlayerNotFound) right $ currentPlayers ^. runLens playerLens
  let currentPlayerStrength =  currentPlayer ^. character . strength
  return $ currentPlayerStrength + head currentDieRolls

loseFight :: (MonadState GameState m) => ReifiedLens' Players (Maybe Player) -> EitherT TalismanErrors m ()
loseFight playerLens = do
  currentPlayers <- gets ( ^. players)
  currentPlayer <- maybe (left PlayerNotFound) right $ currentPlayers ^. runLens playerLens
  let charType = currentPlayer ^. character . characterType
  newPlayers <- traverseOf (at charType)
    (alterMaybePlayer (character . life %~ (1 `subtract`))) currentPlayers
  modify (\gameState -> gameState & players .~ newPlayers )

fight :: (MonadState GameState m) =>  ReifiedLens' Players (Maybe Player) ->
  ReifiedLens' Players (Maybe Player) -> EitherT TalismanErrors m ()
fight attackerLens defenderLens = do
  attackerAttackStrength <-  determineAttackStrength attackerLens
  defenderAttackStrength <-  determineAttackStrength defenderLens
  case compare attackerAttackStrength defenderAttackStrength of
    EQ -> return ()
    LT -> loseFight attackerLens
    GT -> loseFight defenderLens

fightPhase :: (MonadIO m, MonadState GameState m) =>
  Player -> [Player] -> EitherT TalismanErrors m ()
fightPhase player others = do
  maybeFight <- liftIO $ (player ^. ai . selectCharacter) others
--   maybe do this with a traverse in the future, but okay for now
  maybe (return ()) undefined maybeFight
