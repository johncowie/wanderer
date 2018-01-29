module Wanderer.Describable (class Describable, describe) where

class Describable a where
  describe :: a -> String

