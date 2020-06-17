module Imageboard.Types (
    Post(..),
    PostStub(..),
    File(..),
    FileType(..),
    isImage, isAudio,
    Dimensions(..),
    ThreadInfo(..),
    ThreadHead(..),
    Thread(..),
    PostLocation(..),
    Board, BoardInfo(..), BoardConstraints(..),
    Username, AccountInfo(..), Role(..),
    SessionKey
) where
import Data.Text (Text)
import Data.Time.Clock (UTCTime)

-- | Data type representing supported file formats.
data FileType = JPG | PNG | GIF | WEBM | MP4 | MP3 | OGG | PDF | EPUB | SWF 
    deriving (Show, Bounded, Eq)

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
    fromEnum    PDF     = 7
    fromEnum    EPUB    = 8
    fromEnum    SWF     = 9

    toEnum      0       = JPG
    toEnum      1       = PNG
    toEnum      2       = GIF
    toEnum      3       = WEBM
    toEnum      4       = MP4
    toEnum      5       = MP3
    toEnum      6       = OGG
    toEnum      7       = PDF
    toEnum      8       = EPUB
    toEnum      9       = SWF
    toEnum      _       = errorWithoutStackTrace "Enum.FileType: bad argument"

type Board = Text
type SessionKey = Text
type Username = Text

data Role = Admin | Moderator deriving (Show)
instance Enum Role where
    fromEnum    Admin       = 0
    fromEnum    Moderator   = 1

    toEnum      0           = Admin
    toEnum      1           = Moderator
    toEnum      _           = errorWithoutStackTrace "Enum.Role: bad argument"


data AccountInfo = AccountInfo {
        user :: Username
    ,   accountCreated :: UTCTime
    ,   role :: Role
}

data BoardInfo = BoardInfo {
        name :: Board
    ,   title :: Text
    ,   subtitle :: Text
} deriving (Show)

data BoardConstraints = Constraints {
        isLocked :: Bool
    ,   minLen :: Int
    ,   maxLen :: Int
    ,   maxNewLines :: Int
    ,   maxReplies :: Int
    ,   maxThreads :: Int
} deriving (Show)

data PostLocation = PostLocation {
        board :: Board
    ,   number :: Int -- ^ Post number, unique on board.
    ,   parent :: Maybe Int
} deriving (Show)

data Post = Post {  
        loc :: PostLocation
    ,   date :: UTCTime -- ^ UTC date of post creation.
    ,   content :: PostStub 
    ,   file :: Maybe File -- ^ File if attached.
} deriving (Show)

data PostStub = Stub {  
        author :: Text
    ,   email :: Text
    ,   subject :: Text
    ,   text :: Text -- ^ Post body.
} deriving (Show)

data File = File {  
        filename :: Text -- ^ Exact file name, including extension.
    ,   ext :: FileType -- ^ File format.
    ,   size :: Int -- ^ Size in bytes.
    ,   dim :: Maybe Dimensions -- ^ Dimensions if they apply.
} deriving (Show)

data ThreadInfo = ThreadInfo {  
        lastBump :: UTCTime
    ,   sticky :: Bool
    ,   lock :: Bool
    ,   autosage :: Bool
    ,   cycle_ :: Bool
    ,   replyCount :: Int
} deriving (Show)

data ThreadHead = ThreadHead { 
        opPost :: Post
    ,   opInfo :: ThreadInfo
} deriving (Show)

data Thread = Thread {
        op :: ThreadHead
    ,   replies :: [Post]
} deriving (Show)