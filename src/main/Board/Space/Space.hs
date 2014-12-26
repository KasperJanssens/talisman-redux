{-# LANGUAGE  TemplateHaskell #-}
module Board.Space.Space where

import Board.Adventure.Adventure
import Board.Follower.Follower
import Board.Object.Object
import Data.Map
import Control.Lens
import Data.List as List

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


data Space = Space {
  _freeFollowers :: [Follower]
  , _freeObjects :: [Object]
  , _adventures :: [Adventure]
  , _spaceType :: SpaceType
}

makeLenses ''Space

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

{-Initialize previous with current space as it doesn't matter in the beginning anyway, the filtering
should just not happen (in this case it will happen but no space can be it's own neigbour so nothing
will be filtered. It's nasty, it's true. -}
movementOptions :: Int -> SpaceType -> Either String [SpaceType]
movementOptions dieRoll curSpace = movementOptionsEither boardLayout dieRoll curSpace curSpace

movementOptionsEither :: BoardLayout -> Int -> SpaceType -> SpaceType -> Either String [SpaceType]
movementOptionsEither _ 0 previous curSpace = Right [curSpace]
movementOptionsEither layout x previous curSpace = do
                                    neighbours <- maybe
                                                     (Left $ "Didn't find space " ++ show curSpace ++ " in boardLayout, not so good")
                                                     Right $ layout ^. at curSpace
                                    let neighboursButNoBackTracking = List.filter (/= previous) neighbours
                                    listOfLists <- traverse (movementOptionsEither layout (x-1) curSpace) neighboursButNoBackTracking
                                    return . concat $ listOfLists

movementOptionsMaybe :: BoardLayout -> Int -> SpaceType -> SpaceType -> Maybe [SpaceType]
movementOptionsMaybe _ 0 previous curSpace      = Just [curSpace]
movementOptionsMaybe layout x previous curSpace = do
                                    neighbours <- layout ^. at curSpace
                                    let neighboursButNoBackTracking = List.filter (/= previous) neighbours
                                    listOfLists <- traverse (movementOptionsMaybe layout (x-1) curSpace) neighboursButNoBackTracking
                                    return . concat $ listOfLists

createInitialBoard :: Board
createInitialBoard = fromList $ zip fieldsTypeList $
                       fmap createStartingSpace fieldsTypeList
                     where fieldsTypeList = [Fields1Space ..]

createStartingSpace :: SpaceType -> Space
createStartingSpace spaceType = Space {
    _spaceType = spaceType
  , _freeFollowers = []
  , _freeObjects = []
  , _adventures = []
  }


