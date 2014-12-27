{-# LANGUAGE  TemplateHaskell #-}
module Game.GameState where
import Character.Character
import Board.Space.Space
import Board.Adventure.Adventure
import Data.Map
import Control.Lens


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