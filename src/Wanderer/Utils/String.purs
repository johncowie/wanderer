module Wanderer.Utils.String (
  caseInsensitiveEquals
, (=~=)
, elemIgnoreCase
  ) where

import Prelude
import Data.String (toLower)
import Data.Array (null, filter)

caseInsensitiveEquals :: String -> String -> Boolean
caseInsensitiveEquals s1 s2 = toLower s1 == toLower s2

infixr 9 caseInsensitiveEquals as =~=

elemIgnoreCase :: String -> Array String -> Boolean
elemIgnoreCase s = not <<< null <<< filter (caseInsensitiveEquals s)


