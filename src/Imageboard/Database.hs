{-# LANGUAGE OverloadedStrings, TypeOperators #-}
module Imageboard.Database (
    setupDb,
    insertPost,
    insertFile,
    insertPostWithFile,
    insertThread,
    insertThreadWithFile,
    getPosts,
    getThread,
    getThreads,
    checkThread,
    getFileId,
    getConstraints,
    getBoardNames,
    getBoardInfos,
    getPasswordHash,
    insertSessionToken,
    checkSession
) where
import Control.Monad (liftM2, liftM4)
import Data.Text (Text)
import Data.Maybe (listToMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Database.SQLite.Simple as DB
import Database.SQLite.Simple (NamedParam((:=)), (:.)(..))
import qualified Database.SQLite.Simple.FromRow as DB
import qualified Database.SQLite.Simple.FromField as DB (FieldParser, fieldData, returnError)
import qualified Database.SQLite.Simple.Ok as DB (Ok(..))
import qualified Database.SQLite.Simple.Time.Implementation as DB
import qualified Database.SQLite3 as DirectDB (open, close, exec)
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Password.Bcrypt as P (hashPassword, PasswordHash(..),Bcrypt)
import Debug.Trace
import Imageboard.Types

postsDb :: FilePath
postsDb = "posts.db" 

schemaFile :: FilePath
schemaFile = "schema.sql"

parseDate :: DB.FieldParser UTCTime
parseDate f = case DB.fieldData f of 
    -- INTEGER as Unix Time, the number of seconds since 1970-01-01 00:00:00 UTC. 
    DB.SQLInteger x -> DB.Ok . posixSecondsToUTCTime $ fromIntegral x
    -- TEXT as ISO8601 strings ("YYYY-MM-DD HH:MM:SS.SSS"). 
    DB.SQLText x -> case DB.parseUTCTime x of 
        Left s -> DB.returnError DB.ConversionFailed f s
        Right t -> DB.Ok $ t
    _ -> DB.returnError DB.Incompatible f "expecting an SQLInteger column type"

mkThreadHead ::  Post :. ThreadInfo -> ThreadHead
mkThreadHead (p :. i) = ThreadHead p i

instance DB.FromRow Post where
    fromRow = Post  <$> (PostLocation
                            <$> DB.field
                            <*> DB.field
                            <*> DB.field)
                    <*> DB.fieldWith parseDate 
                    <*> (Stub   
                        <$> DB.field 
                        <*> DB.field 
                        <*> DB.field 
                        <*> DB.field)
                    <*> (liftM4 File    
                        <$> DB.field 
                        <*> ((toEnum <$>) <$> DB.field) 
                        <*> DB.field 
                        <*> (Just <$> (liftM2 Dim 
                            <$> DB.field
                            <*> DB.field)))
        where mkPost _ = Post -- ignore Parent column

instance DB.FromRow ThreadInfo where
    fromRow = ThreadInfo 
                <$> DB.fieldWith parseDate
                <*> DB.field
                <*> DB.field 
                <*> DB.field
                <*> DB.field
                <*> DB.field

instance DB.FromRow BoardConstraints where
    fromRow = Constraints
                <$> DB.field
                <*> DB.field 
                <*> DB.field
                <*> DB.field
                <*> DB.field
                <*> DB.field 

instance DB.FromRow BoardInfo where
    fromRow = BoardInfo <$> DB.field <*> DB.field <*> DB.field 

-- | Create sqlite3 database file using schema file.
setupDb :: IO ()
setupDb = do
    conn <- DirectDB.open $ T.pack postsDb
    schema <- T.readFile schemaFile
    DirectDB.exec conn schema
    hash <- P.hashPassword "password"
    -- Insert default admin account.
    DB.executeNamed (DB.Connection conn)   
        "INSERT OR IGNORE INTO Accounts (Username, Password, Role) \
        \VALUES (:user, :hash, 'admin')"
        [ ":user" := ("admin" :: Text)
        , ":hash" := unPasswordHash hash] 
    DirectDB.close conn


-- | Insert file information into database.
insertFile :: File -> IO Int -- ^ File Id.
insertFile f = DB.withConnection postsDb $ \c -> do
    DB.executeNamed c   "INSERT INTO Files (Name, Extension, Size, Width, Height) \
                        \VALUES (:name, :ext, :size, :width, :height)" 
                        [ ":name"   := filename f
                        , ":ext"    := (fromEnum $ ext f)
                        , ":size"   := size f
                        , ":width"  := width <$> dim f
                        , ":height" := height <$> dim f]
    fromIntegral <$> DB.lastInsertRowId c

-- | Insert post with no file attached into database.
insertPost :: Board -> Int -> PostStub -> IO () 
insertPost b p s = DB.withConnection postsDb $ \c -> do
    DB.executeNamed c   "INSERT INTO Posts (Name, Email, Subject, Text, Parent, Board) \
                        \VALUES (:name, :email, :subject, :text, :parent, :board)" 
                        [ ":name"       := author s
                        , ":email"      := email s
                        , ":subject"    := subject s
                        , ":text"       := text s
                        , ":parent"     := p
                        , ":board"      := b]

-- | Insert post with file attached, using previously obtained file Id.
insertPostWithFile :: Board -> Int -> PostStub -> Int -- ^ fileId 
                    -> IO () -- ^ Post Id.
insertPostWithFile b p s f = DB.withConnection postsDb $ \c -> do
    DB.executeNamed c   "INSERT INTO Posts (Name, Email, Subject, Text, FileId, Parent, Board) \
                        \VALUES (:name, :email, :subject, :text, :fileid, :parent, :board)" 
                        [ ":name"       := author s
                        , ":email"      := email s
                        , ":subject"    := subject s
                        , ":text"       := text s
                        , ":fileid"     := f
                        , ":parent"     := p
                        , ":board"      := b]

insertThread :: Board -> PostStub -> IO ()
insertThread b s = DB.withConnection postsDb $ \c -> do
    DB.executeNamed c   "INSERT INTO Posts (Name, Email, Subject, Text, Board) \
                        \VALUES (:name, :email, :subject, :text, :board)" 
                        [ ":name"       := author s
                        , ":email"      := email s
                        , ":subject"    := subject s
                        , ":text"       := text s
                        , ":board"      := b]

insertThreadWithFile :: Board -> PostStub -> Int -> IO ()
insertThreadWithFile b s f = DB.withConnection postsDb $ \c -> do
    DB.executeNamed c   "INSERT INTO Posts (Name, Email, Subject, Text, FileId, Board) \
                        \VALUES (:name, :email, :subject, :text, :fileid, :board)" 
                        [ ":name"       := author s
                        , ":email"      := email s
                        , ":subject"    := subject s
                        , ":text"       := text s
                        , ":fileid"     := f
                        , ":board"      := b]

-- | Obtain ID of previously inserted file.
getFileId :: Text -> IO (Maybe Int) -- ^ File Id, if given filename exists in database.
getFileId name = DB.withConnection postsDb $ \c -> do
    l <- DB.queryNamed c    "SELECT Id FROM Files WHERE Name = :name" 
                            [":name" := name] :: IO [DB.Only Int]
    return $ DB.fromOnly <$> listToMaybe l

getPosts :: Int -> IO [Post]
getPosts n = DB.withConnection postsDb $ \c ->
    DB.queryNamed c     "SELECT * FROM posts_and_files ORDER BY Date DESC LIMIT :n" 
                        [":n" := n]

getThread :: Board -> Int -> IO (Maybe Thread)
getThread b n = DB.withConnection postsDb $ \c -> do
    h <-  DB.queryNamed c   "SELECT * FROM threads WHERE Number = :number AND Board = :board" 
                            [":number" := n, ":board" := b]
    ps <- DB.queryNamed c   "SELECT * FROM posts_and_files WHERE Parent = :number \
                            \AND Board = :board ORDER BY Number ASC"
                            [":number" := n, ":board" := b] 
    case h of
        [] -> return $ Nothing
        x:_ -> return $ Just $ Thread (mkThreadHead x) ps

checkThread :: Board -> Int -> IO Bool
checkThread b n = DB.withConnection postsDb $ \c -> do
    h <-  DB.queryNamed c   "SELECT EXISTS\
                            \(SELECT 1 FROM threads WHERE Number = :number AND Board = :board)" 
                            [":number" := n, ":board" := b] :: IO [DB.Only Bool]
    return $ any DB.fromOnly h

getThreads :: Board -> IO [ThreadHead]
getThreads b = DB.withConnection postsDb $ \c -> do
    q <- DB.queryNamed c    "SELECT * FROM threads WHERE Board = :board \
                            \ORDER BY Sticky DESC, LastBump DESC" 
                            [":board" := b]
    return $ map mkThreadHead q

getConstraints :: Board -> IO (Maybe BoardConstraints)
getConstraints b = DB.withConnection postsDb $ \c -> do
    bc <- DB.queryNamed c   "SELECT Lock, PostMinLength, PostMaxLength, PostMaxNewlines, \
                            \PostLimit, ThreadLimit FROM Boards WHERE Name = :board"
                            [":board" := b] :: IO [BoardConstraints]
    return $  listToMaybe bc

getBoardNames :: IO [Board]
getBoardNames = DB.withConnection postsDb $ \c ->
    (fmap . fmap) DB.fromOnly $ DB.query_ c "SELECT Name From Boards"

getBoardInfos :: IO [BoardInfo]
getBoardInfos = DB.withConnection postsDb $ \c ->
    DB.query_ c "SELECT Name, Title, Subtitle From Boards"

createAccount :: Text -> PasswordHash Bcrypt -> IO Bool
createAccount u h = DB.withConnection postsDb $ \c -> do
    DB.executeNamed c   "INSERT INTO Accounts (Username, Password) \
                        \VALUES (:name, :hash)" 
                        [ ":name"   := u
                        , ":hash"   := unPasswordHash h]
    return True
        
getPasswordHash :: Text -> IO (Maybe (PasswordHash Bcrypt))
getPasswordHash user =  DB.withConnection postsDb $ \c -> do
    q <- DB.queryNamed c    "SELECT Password From Accounts WHERE USERNAME = :user"
                            [":user" := user] :: IO [DB.Only Text]
    return $ PasswordHash <$> DB.fromOnly <$> listToMaybe q

insertSessionToken :: Text -> Text -> IO ()
insertSessionToken a t = DB.withConnection postsDb $ \c -> do
    DB.executeNamed c   "INSERT INTO Sessions (Username, Key) \
                        \VALUES (:name, :token)" 
                        [ ":name"   := a
                        , ":token"  := t]

checkSession :: Text -> IO Bool
checkSession t = DB.withConnection postsDb $ \c -> do
    q <- DB.queryNamed c   "SELECT 1 FROM Sessions WHERE Key = :token"
                            [":token" := t]  :: IO [DB.Only Bool]
    return $ any DB.fromOnly q
