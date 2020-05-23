module Imageboard.Types (
    Post(..),
    File(..)
) where
import Data.Text (Text)
import Data.Time.Clock (UTCTime)

data Post = Post {
    number :: Int,
    date :: UTCTime,
    author :: Text,
    email :: Text,
    subject :: Text,
    text :: Text
} deriving (Show)

data File = File {
    filename :: Text,
    size :: Int,
    width :: Int,
    height :: Int
} deriving (Show)
