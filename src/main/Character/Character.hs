module Character.Character where

import Board.Object.Object
import Board.Follower.Follower
import Board.Adventure.Adventure
import Control.Lens
import Data.Map
import Board.Space.Space

data Alignment = Good | Neutral | Evil deriving (Eq, Ord, Show)

data Character = Character {
  _strength :: Int
  , _craft :: Int
  , _fate :: Int
  , _gold :: Int
  , _life :: Int
  , _objects :: [Object]
  , _followers :: [Follower]
  , _alignment :: Alignment
  , _place :: ReifiedLens' (Map Int Space) Space
}
