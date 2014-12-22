module Board.Space.Space where

import Board.Adventure.Adventure
import Board.Follower.Follower
import Board.Object.Object
import Data.Map
import Control.Lens
import Control.Monad

data Space = Space {
  _freeFollowers :: [Follower]
  , _freeObjects :: [Object]
  , _adventures :: [Adventure]
  , _type :: SpaceType
}

type Board = Map SpaceType Space
type BoardLayout = Map SpaceType [SpaceType]

boardLayout :: BoardLayout
boardLayout = fromList [
    (ChapelSpace ,   [Hills1Space, Fields6Space])
  , (Hills1Space ,   [ChapelSpace, SentinelSpace])
  , (SentinelSpace,  [Hills1Space, Woods1Space])
  , (Woods1Space,    [SentinelSpace, GraveyardSpace])
  , (GraveyardSpace, [Woods1Space, Fields1Space])
  , (Fields1Space,   [GraveyardSpace, VillageSpace])
  , (VillageSpace,   [Fields1Space, Fields2Space])
  , (Fields2Space,   [VillageSpace, ForestSpace])
  , (ForestSpace,    [Fields2Space, Plains1Space])
  , (Plains1Space,   [ForestSpace, RuinsSpace])
  , (RuinsSpace,     [Plains1Space, Fields3Space])
  , (Fields3Space,   [RuinsSpace, TavernSpace])
  , (TavernSpace,    [Fields3Space, Plains2Space])
  , (Plains2Space,   [TavernSpace, Woods2Space])
  , (Woods2Space,    [Plains2Space, Plains3Space])
  , (Plains3Space,   [Woods2Space, Hills2Space])
  , (Hills2Space,    [Plains3Space, Fields4Space])
  , (Fields4Space,   [Hills2Space, CitySpace])
  , (CitySpace,      [Fields4Space, Fields5Space])
  , (Fields5Space,   [CitySpace, Woods3Space])
  , (Woods3Space,    [Fields5Space, Plains4Space])
  , (Plains4Space,   [Woods3Space, CragsSpace])
  , (CragsSpace,     [Plains4Space, Fields6Space])
  , (Fields6Space,   [CragsSpace, ChapelSpace])
 ]

movementOptions :: BoardLayout -> Int -> SpaceType -> Maybe [SpaceType]
movementOptions _ 0 curSpace      = Just [curSpace]
movementOptions layout x curSpace = neighbours
                                      & fmap (traverse $ movementOptions layout (x-1))
                                      & join
                                      & fmap concat
                                    where neighbours = layout ^. at curSpace

createInitialBoard :: Map SpaceType Space
createInitialBoard = fromList $ zip fieldsTypeList $
                       fmap createStartingSpace fieldsTypeList
                     where fieldsTypeList = [Fields1Space ..]

createStartingSpace :: SpaceType -> Space
createStartingSpace spaceType = Space {
    _type = spaceType
  , _freeFollowers = []
  , _freeObjects = []
  , _adventures = []
  }

data SpaceType =
  Fields1Space 
  | Fields2Space 
  | Fields3Space 
  | Fields4Space 
  | Fields5Space 
  | Fields6Space 
  | ForestSpace 
  | RuinsSpace 
  | TavernSpace 
  | Plains1Space 
  | Plains2Space 
  | Plains3Space 
  | Plains4Space 
  | Woods1Space 
  | Woods2Space 
  | Woods3Space 
  | Hills1Space 
  | Hills2Space 
  | CitySpace 
  | ChapelSpace 
  | SentinelSpace 
  | GraveyardSpace 
  | VillageSpace 
  | CragsSpace  deriving (Eq, Ord, Show, Enum)

