{-# LANGUAGE  FlexibleContexts #-}
module DungeonMaster.DungeonMaster where

import Control.Monad.State
import Control.Monad.Trans.Either
import Character.Character
import Board.Space.Space
import Control.Lens
import qualified Data.List as List
import Data.Map
import Game.GameState
import Board.Adventure.Adventure
import Data.Foldable
import TalismanErrors.TalismanErrors

nextRoll:: (MonadState [DieRoll] m , Monad m) => m DieRoll
nextRoll = do
    rolls <- get
    let nextOne = head rolls
    put $ tail rolls
    return nextOne

movePhase :: (MonadState GameState m, MonadIO m) =>
  ReifiedLens' Players Player -> EitherT TalismanErrors m ()
movePhase playerLens  = do
   currentPlayers <- gets (^. players)
   let currentPlace = currentPlayers ^.  runLens playerLens . place
       selectSpaceFunction = currentPlayers ^. runLens playerLens . ai . selectSpace
   currentDieRolls <- gets ( ^. dieRolls)
   options <- hoistEither $ movementOptions (head currentDieRolls) currentPlace
   board <- gets ( ^. board )
   spaces <- hoistEither $ lookupSpaces options board
   selectedSpace <- liftIO $ selectSpaceFunction spaces
   let selectedSpaceType = selectedSpace ^. spaceType
   modify (\gameState -> gameState
       & players . runLens playerLens . place .~ selectedSpaceType
       & dieRolls %~ tail
    )



determineAttackStrength :: (MonadState GameState m) => ReifiedLens' Players Player ->
 EitherT String m Int
determineAttackStrength playerLens = do
  currentDieRolls <- gets (^. dieRolls)
  currentPlayers <- gets (^. players)
  modify ( & dieRolls %~ tail )
  let currentPlayerStrength = currentPlayers ^. runLens playerLens . character . strength
  return $ currentPlayerStrength + head currentDieRolls

alterMaybePlayer :: (MonadState GameState m) => (Player -> Player) ->
  Maybe Player -> EitherT String m (Maybe Player)
alterMaybePlayer alterPlayer maybePlayer =
  hoistEither $ maybe (Left "Player not found") (Right . Just . alterPlayer) maybePlayer

loseFight :: (MonadState GameState m) => ReifiedLens' Players Player -> EitherT String m ()
loseFight playerLens = do
  currentPlayers <- gets ( ^. players)
  let charType = currentPlayers ^. runLens playerLens . character . characterType
  newPlayers <- traverseOf (at charType)
    (alterMaybePlayer (character . life %~ (`subtract` 1))) currentPlayers
  modify (\gameState -> gameState & players .~ newPlayers )

fight :: (MonadState GameState m) =>  ReifiedLens' Players Player ->
  ReifiedLens' Players Player -> EitherT String m ()
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

determineNumberOfCardsToDraw :: (MonadState GameState m, MonadIO m) =>
  ReifiedLens' Players Player -> EitherT TalismanErrors m Int
determineNumberOfCardsToDraw playerLens = do
  players <- gets (^. players)
  let currentSpaceLens  = players ^. runLens playerLens . place
  board <- gets (^.  board)
  currentSpace <- hoistEither $ maybe (Left SpaceTypeNotFound) Right $  board ^. at currentSpaceLens
  let maxCards = cardsToDraw currentSpace
  let currentCards =  length $ currentSpace ^. adventures
  case maxCards `compare` currentCards of
    GT -> return $ maxCards - currentCards
    _ -> return 0

alterMaybeSpace :: (MonadState GameState m) => (Space -> Space) ->
  Maybe Space -> EitherT TalismanErrors m (Maybe Space)
alterMaybeSpace alterSpace maybeSpace =
  hoistEither $ maybe (Left SpaceTypeNotFound) (Right .Just . alterSpace) maybeSpace

addAdventures :: (MonadState GameState m) => ReifiedLens' Board (Maybe Space)
  -> [Adventure] -> EitherT TalismanErrors m ()
addAdventures  spaceLens newAdventures = do
  currentBoard <- gets ( ^. board)
  newBoard <-  traverseOf (runLens spaceLens)
    (alterMaybeSpace (adventures %~ (newAdventures ++))) currentBoard
  modify (\gameState -> board .~ newBoard $ gameState )


drawCards :: (MonadState GameState m, MonadIO m) => ReifiedLens' Players Player -> EitherT TalismanErrors m ()
drawCards playerLens = do
  numberOfCardsToDraw <- determineNumberOfCardsToDraw  playerLens
  currentPlayers <- gets (^. players)
  currentAdventureStack <- gets ( ^. adventureStack)
  let (drawnCards, rest) = splitAt numberOfCardsToDraw currentAdventureStack
  modify (adventureStack .~ rest)
  addAdventures (Lens $ at $ currentPlayers ^. runLens playerLens . place ) drawnCards

playRound :: (MonadState GameState m, MonadIO m) => ReifiedLens' Players Player
 -> EitherT TalismanErrors m ()
playRound playerLens = do
  movePhase playerLens
  players <- gets (^. players)
  let player = players ^.  runLens playerLens
--   otherPlayersOnSameSpace <- getOtherPlayersInSamePosition player
-- TODO
  otherPlayersOnSameSpace <- getOtherPlayersInSamePosition undefined
  fightPhase player otherPlayersOnSameSpace
  drawCards playerLens