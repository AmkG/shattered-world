module Game.ShatteredWorld.Object(Object(..), ObjectId(..)) where

import Data.Word

data Object = Object {objectId :: ObjectId } -- TODO

newtype ObjectId = ObjectId Word32
  deriving(Read, Show, Ord, Eq)

instance Enum ObjectId where
  fromEnum (ObjectId w) = fromEnum w
  toEnum i = ObjectId (toEnum i)

