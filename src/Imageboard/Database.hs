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
    getFileId
) where
import Control.Monad (liftM2, liftM4)
import Data.Text (Text)
import Data.Maybe (listToMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Database.SQLite.Simple as DB
import Database.SQLite.Simple (NamedParam((:=)), (:.)(..))
import qualified Database.SQLite.Simple.FromRow as DB (RowParser, fieldWith)
import qualified Database.SQLite.Simple.FromField as DB (FieldParser, fieldData, returnError)
import qualified Database.SQLite.Simple.Ok as DB (Ok(..))
import qualified Database.SQLite.Simple.Time.Implementation as DB
import qualified Database.SQLite3 as DirectDB (open, close, exec)
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
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
    fromRow = Post <$> DB.field 
                    <*> DB.field
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

-- | Create sqlite3 database file using schema file.
setupDb :: IO ()
setupDb = do
    conn <- DirectDB.open $ T.pack postsDb
    schema <- T.readFile schemaFile
    DirectDB.exec conn schema
    DirectDB.close conn

-- | Insert post with no file attached into database.
insertPost :: Int -> PostStub -> IO Int -- ^ Post Id.
insertPost p s = DB.withConnection postsDb $ \c -> do
    DB.executeNamed c   "INSERT INTO Posts (Name, Email, Subject, Text, Parent) \
                        \VALUES (:name, :email, :subject, :text, :parent)" 
                        [ ":name"       := author s
                        , ":email"      := email s
                        , ":subject"    := subject s
                        , ":text"       := text s
                        , ":parent"     := p]
    fromIntegral <$> DB.lastInsertRowId c

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

-- | Insert post with file attached, using previously obtained file Id.
insertPostWithFile :: Int -> PostStub -> Int -- ^ fileId 
                    -> IO Int -- ^ Post Id.
insertPostWithFile p s f = DB.withConnection postsDb $ \c -> do
    DB.executeNamed c   "INSERT INTO Posts (Name, Email, Subject, Text, FileId, Parent) \
                        \VALUES (:name, :email, :subject, :text, :fileid, :parent)" 
                        [ ":name"       := author s
                        , ":email"      := email s
                        , ":subject"    := subject s
                        , ":text"       := text s
                        , ":fileid"     := f
                        , ":parent"     := p]
    fromIntegral <$> DB.lastInsertRowId c

insertThread :: PostStub -> IO Int
insertThread s = DB.withConnection postsDb $ \c -> do
    DB.executeNamed c   "INSERT INTO Posts (Name, Email, Subject, Text) \
                        \VALUES (:name, :email, :subject, :text)" 
                        [ ":name"       := author s
                        , ":email"      := email s
                        , ":subject"    := subject s
                        , ":text"       := text s]
    n <- DB.lastInsertRowId c
    DB.executeNamed c   "INSERT INTO ThreadInfo (Number) VALUES (:number)"
                        [":number"      := n]
    return $ fromIntegral n

insertThreadWithFile :: PostStub -> Int -> IO Int
insertThreadWithFile s f = DB.withConnection postsDb $ \c -> do
    DB.executeNamed c   "INSERT INTO Posts (Name, Email, Subject, Text, FileId) \
                        \VALUES (:name, :email, :subject, :text, :fileid)" 
                        [ ":name"       := author s
                        , ":email"      := email s
                        , ":subject"    := subject s
                        , ":text"       := text s
                        , ":fileid"     := f]
    n <- DB.lastInsertRowId c
    DB.executeNamed c   "INSERT INTO ThreadInfo (Number) VALUES (:number)"
                        [":number"      := n]
    return $ fromIntegral n

-- | Obtain file Id of previously inserted file, giving its name.
getFileId :: Text -> IO (Maybe Int) -- ^ File Id, if given filename exists in database.
getFileId name = DB.withConnection postsDb $ \c -> do
    l <- DB.queryNamed c "SELECT Id FROM Files WHERE Name = :name" 
        [":name" := name] :: IO [DB.Only Int]
    return $ DB.fromOnly <$> listToMaybe l

getPosts :: IO [Post]
getPosts = DB.withConnection postsDb $ \c ->
    DB.query_ c "SELECT * FROM posts_and_files" 

getThread :: Int -> IO (Either Text Thread)
getThread n = DB.withConnection postsDb $ \c -> do
    h <-  DB.queryNamed c "SELECT * FROM threads WHERE Number = :number" 
        [":number" := n]
    ps <- DB.queryNamed c   "SELECT * FROM posts_and_files WHERE posts_and_files.Parent = :number \
                            \ORDER BY Number ASC"
        [":number" := n] 
    case h of
        [] -> return $ Left "No such thread"
        x:_ -> return $ Right $ Thread (mkThreadHead x) ps
checkThread :: Int -> IO Bool
checkThread n = DB.withConnection postsDb $ \c -> do
    h <-  DB.queryNamed c "SELECT EXISTS(SELECT 1 FROM threads WHERE Number = :number)" 
        [":number" := n] :: IO [DB.Only Bool]
    return $ any DB.fromOnly h

getThreads :: IO [ThreadHead]
getThreads = DB.withConnection postsDb $ \c -> do
    q <- DB.query_ c    "SELECT * FROM threads \
                        \ORDER BY Sticky DESC, LastBump DESC" 
    return $ map mkThreadHead q