module Wanderer.Data.Room
( Room
, location
, findRoom
, moveLocation
  ) where

import Prelude
import Wanderer.Utils.String ((=~=), elemIgnoreCase)
import Wanderer.Describable (class Describable)
import Data.Either (Either(..))
import Data.Array (filter)

newtype Room = Room { roomName :: String
                    , roomSynonyms :: Array String
                    , roomDescription :: String
                    , adjacentRooms :: Array Room
                    }

roomSynonyms :: Room -> Array String
roomSynonyms (Room r) = r.roomSynonyms

roomName :: Room -> String
roomName (Room r) = r.roomName

adjacentRooms :: Room -> Array Room
adjacentRooms (Room r) = r.adjacentRooms

instance describeRoom :: Describable Room where
  describe (Room r) = r.roomDescription

location :: String -> Array String -> String -> Array Room -> Room
location a b c d = Room { roomName: a, roomSynonyms: b, roomDescription: c, adjacentRooms: d}

findRoom :: String -> Array Room -> Either String Room
findRoom s rooms =
  case filter (((=~=) s) <<< roomName) rooms of
    [x] -> Right x
    xs  -> case filter (elemIgnoreCase s <<< roomSynonyms) rooms of
      [x] -> Right x
      []  -> Left "No room that you can move to with that name"
      _   -> Left "Not sure what room you meant! Multiple rooms match that description.."

moveLocation :: String -> Room -> Either String Room
moveLocation s as = findRoom s (adjacentRooms as)
