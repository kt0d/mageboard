module Imageboard.Types (
    Post(..),
    PostStub(..),
    File(..),
) where
import Data.Text (Text)
import Data.Time.Clock (UTCTime)

data FileType = Image | Video | Audio | Document
    deriving (Show)

data Post = Post {
    number :: Int,
    date :: UTCTime,
    content :: PostStub,
    file :: Maybe File
} deriving (Show)

data PostStub = Stub {
    author :: Text,
    email :: Text,
    subject :: Text,
    text :: Text
} deriving (Show)

data File = File {
    filename :: Text,
    extension :: Text,
    size :: Int,
    width :: Maybe Int,
    height :: Maybe Int
} deriving (Show)
