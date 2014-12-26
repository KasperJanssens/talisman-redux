{-# LANGUAGE TemplateHaskell #-}
module Character.Character where

import Board.Object.Object
import Board.Follower.Follower
import Board.Adventure.Adventure
import Control.Lens
import Board.Space.Space

data Alignment = Good | Neutral | Evil deriving (Eq, Ord, Show)

data CharacterType = Wizard | Thief | OgreChieftain

data Character = Character {
  _strength :: Int
  , _craft :: Int
  , _fate :: Int
  , _gold :: Int
  , _life :: Int
  , _objects :: [Object]
  , _followers :: [Follower]
  , _alignment :: Alignment
  , _characterType :: CharacterType
}

data AI = AI {
  _selectCharacter :: [Character] -> IO Character
  , _selectSpace :: [Space] -> IO Space

}

makeLenses ''AI

sillyAI :: AI
sillyAI = AI {
  _selectCharacter = return . head
  , _selectSpace = return . head
}

{- Maybe place has to move to Character??-}
data Player = Player {
  _character :: Character
  , _place :: SpaceType
  , _ai :: AI
}

makeLenses ''Player

wizard :: AI -> Player
wizard ai = Player {
  _character = Character {
    _strength=2,
    _craft=5,
    _fate=3,
    _gold=1,
    _life=4,
    _objects=[],
    _followers=[],
    _alignment=Evil,
    _characterType = Wizard
    }
  , _place = GraveyardSpace
  , _ai = ai
}

ogreChieftain::AI -> Player
ogreChieftain ai = Player {
  _character = Character {
    _strength=5,
    _craft=2,
    _fate=1,
    _gold=1,
    _life=6,
    _objects=[],
    _followers=[],
    _alignment=Neutral,
    _characterType = OgreChieftain
    }
  , _place=CragsSpace
  , _ai = ai
}

thief :: AI -> Player
thief ai = Player {
  _character = Character {
    _strength=3,
    _craft=3,
    _fate=2,
    _gold=1,
    _life=4,
    _objects=[],
    _followers=[],
    _alignment=Neutral,
    _characterType = Thief
    }
  , _place= CitySpace
  , _ai = ai
}


