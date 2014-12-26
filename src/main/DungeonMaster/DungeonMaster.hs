{-# LANGUAGE  FlexibleContexts #-}
module DungeonMaster.DungeonMaster where

import Control.Monad.State
import Control.Monad.Trans.Either
import Character.Character
import Board.Space.Space
import Control.Lens
import qualified Data.List as List
import Data.Map

nextRoll:: (MonadState [DieRoll] m , Monad m) => m DieRoll
nextRoll = do
    rolls <- get
    let nextOne = head rolls
    put $ tail rolls
    return nextOne

type DieRoll = Int

type Players = Map CharacterType Player

type Game = (Players, Board)

getOtherPlayersInSamePosition :: Player -> [Player] -> [Player]
getOtherPlayersInSamePosition selectedPlayer = List.filter (\player -> player ^. place == selectedCharPlace)
  where selectedCharPlace = selectedPlayer ^. place

lookupSpaces :: [SpaceType] -> Board -> Either String [Space]
lookupSpaces spaceTypes board = traverse
  (\spaceType -> maybe (Left "spaceType not found") Right $ board ^. at spaceType)
  spaceTypes

move :: (MonadState (Game,DieRolls) m, MonadIO m) =>
  ReifiedLens' Players Player -> EitherT String m SpaceType
move playerLens  = do
               (game,dieRolls) <- get
               let currentPlace = game ^. _1 . runLens playerLens . place
                   selectSpaceFunction = game ^. _1 . runLens playerLens . ai . selectSpace
               options <- hoistEither $ movementOptions (head dieRolls) currentPlace
               spaces <- hoistEither $ lookupSpaces options $ game ^. _2
               selectedSpace <- liftIO $ selectSpaceFunction spaces
               let selectedSpaceType = selectedSpace ^. spaceType
               let newGameSituation =  _1 . runLens playerLens . place .~ selectedSpaceType $ game
               put (newGameSituation, tail dieRolls)
               return selectedSpaceType

type DieRolls = [DieRoll]

playRound :: ReifiedLens' Players Player -> EitherT String (StateT (Game, DieRolls) IO) ()
playRound playerLens = do
  (game,dieRolls) <- get
  let dieRoll = head dieRolls
  movedToSpaceType <- move playerLens
  return ()