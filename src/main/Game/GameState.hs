{-# LANGUAGE  TemplateHaskell #-}
{-# LANGUAGE  FlexibleContexts #-}
module Game.GameState where

import Character.Character
import Board.Space.Space
import Board.Adventure.Adventure
import Data.Map
import Control.Lens
import Control.Monad.State
import Control.Monad.Trans.Either
import Data.Foldable
import TalismanErrors.TalismanErrors


type DieRoll = Int

type Players = Map CharacterType Player

type DieRolls = [DieRoll]


data GameState = GameState {
  _players :: Players,
  _board :: Board,
  _dieRolls :: DieRolls,
  _adventureStack :: [Adventure]
}

makeLenses ''GameState

getOtherPlayersInSamePosition :: (MonadState GameState m) =>
  ReifiedLens' Players (Maybe Player) -> EitherT TalismanErrors m [Player]
getOtherPlayersInSamePosition playerLens = do
  currentPlayers <- gets ( ^. players)
  selectedPlayerPlace <- maybe (left PlayerNotFound) (right .  (^. place) )
     $ currentPlayers ^. runLens playerLens
  selectedPlayerType <- maybe (left PlayerNotFound) (right . (^. character . characterType))
     $ currentPlayers ^. runLens playerLens
  return $ foldMap (\p -> case (p ^. place) `compare` selectedPlayerPlace of
                            EQ -> case (p ^. character . characterType) `compare` selectedPlayerType of
                                    EQ -> []
                                    _  -> [p]
                            _ -> []
                          ) currentPlayers