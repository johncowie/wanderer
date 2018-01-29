module App where

import Prelude
import Wanderer.Data.Room (location, Room, moveLocation)
import Wanderer.Data.Command (Command(..))
import Wanderer.Data.Game (Game)
import Data.Either (Either(..))
import Data.List (List, (:))
import Data.List as L
import Data.Array (fromFoldable)
import Data.String (joinWith, split, Pattern(..))

-- import Unsafe.Coerce (unsafeCoerce)

room1 :: Room
room1 = location "room1" ["room 1", "1"] "You are in room1! Room 2 and Room 3 are adjacent." []

room2 :: Room
room2 = location "room2" ["room 2", "2"] "You are in room2! Room 1 is adjacent." []

room3 :: Room
room3 = location "room3" ["room 3", "3"] "You are in room3! Room 1 is adjacent." []

processCommand :: Command -> Game -> Either String Game
processCommand (Move loc) = moveLocation loc

parseWords :: List String -> Either String Command
parseWords ("go":"to":xs) = Right $ Move (unwords xs)
parseWords _ = Left "Didn't understand what you wanted! :("

parseCommand :: String -> Either String Command
parseCommand = parseWords <<< words

-- TODO move to string utils
unwords :: List String -> String
unwords = joinWith " " <<< fromFoldable

words :: String -> List String
words = L.fromFoldable <<< split (Pattern "\\s+")
