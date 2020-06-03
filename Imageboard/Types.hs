module Imageboard.Types (
    Post(..),
    PostStub(..),
    File(..),
    FileType(..),
    isImage,
    Dimensions(..)
) where
import Data.Text (Text)
import Data.Time.Clock (UTCTime)

data FileType = JPG | PNG | GIF | WEBM | MP4 | MP3 | OGG deriving (Show, Bounded)
isImage :: FileType -> Bool
isImage JPG = True
isImage PNG = True
isImage GIF = True
isImage _ = False
data Dimensions = Dim { width :: Int, height :: Int} deriving (Show)

instance Enum FileType where
    fromEnum    JPG     = 0
    fromEnum    PNG     = 1
    fromEnum    GIF     = 2
    fromEnum    WEBM    = 3
    fromEnum    MP4     = 4
    fromEnum    MP3     = 5
    fromEnum    OGG     = 6

    toEnum      0       = JPG
    toEnum      1       = PNG
    toEnum      2       = GIF
    toEnum      3       = WEBM
    toEnum      4       = MP4
    toEnum      5       = MP3
    toEnum      6       = OGG

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
    ext :: FileType,
    size :: Int,
    dim :: Maybe Dimensions
} deriving (Show)
