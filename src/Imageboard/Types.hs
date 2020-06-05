module Imageboard.Types (
    Post(..),
    PostStub(..),
    File(..),
    FileType(..),
    isImage, isAudio,
    Dimensions(..)
) where
import Data.Text (Text)
import Data.Time.Clock (UTCTime)

-- | Data type representing supported file formats.
data FileType = JPG | PNG | GIF | WEBM | MP4 | MP3 | OGG deriving (Show, Bounded, Eq)

-- | Whether given file format may store an image.
isImage :: FileType -> Bool
isImage JPG = True
isImage PNG = True
isImage GIF = True
isImage _ = False

-- | Whether given file format may store an audio. 
isAudio :: FileType -> Bool
isAudio MP3 = True
isAudio OGG = True
isAudio _ = False

-- | Dimensions of raster image or video.
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
    toEnum      _       = errorWithoutStackTrace "Enum.FileType: bad argument"

data Post = Post {  number :: Int   -- ^ Post number, unique on board.
                ,   date :: UTCTime -- ^ UTC date of post creation.
                ,   content :: PostStub 
                ,   file :: Maybe File -- ^ File if attached.
} deriving (Show)

data PostStub = Stub {  author :: Text
                    ,   email :: Text
                    ,   subject :: Text
                    ,   text :: Text -- ^ Post body.
} deriving (Show)

data File = File {  filename :: Text -- ^ Exact file name, including extension.
                ,   ext :: FileType -- ^ File format.
                ,   size :: Int -- ^ Size in bytes.
                ,   dim :: Maybe Dimensions -- ^ Dimensions if they apply.
} deriving (Show)
