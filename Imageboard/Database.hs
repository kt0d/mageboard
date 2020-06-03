{-# LANGUAGE OverloadedStrings #-}
module Imageboard.Database (
    setupDb,
    insertPost,
    insertFile,
    insertPostWithFile,
    getPosts,
    getFileId
) where
import Control.Monad (liftM2, liftM4, liftM)
import Data.Text (Text)
import Data.Maybe (listToMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Database.SQLite.Simple as DB
import Database.SQLite.Simple (NamedParam((:=)))
import qualified Database.SQLite.Simple.FromRow as DB (fieldWith, RowParser)
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

instance DB.FromRow Post where
    fromRow = Post  <$> DB.field 
                    <*> DB.fieldWith parseDate 
                    <*> (Stub   <$> DB.field 
                                <*> DB.field 
                                <*> DB.field 
                                <*> DB.field)
                    <*> (liftM4 File    <$> DB.field 
                                        <*> ((toEnum <$>) <$> DB.field) 
                                        <*> DB.field 
                                        <*> (Just <$> (liftM2 Dim <$> DB.field
                                                                <*> DB.field)))

setupDb :: IO ()
setupDb = do
    conn <- DirectDB.open $ T.pack postsDb
    schema <- T.readFile schemaFile
    DirectDB.exec conn schema
    DirectDB.close conn

insertPost :: PostStub -> IO Int
insertPost s = DB.withConnection postsDb $ \c -> do
    DB.executeNamed c   "INSERT INTO Posts (Name, Email, Subject, Text) \
                        \VALUES (:name, :email, :subject, :text)" 
                        [ ":name"       := author s
                        , ":email"      := email s
                        , ":subject"    := subject s
                        , ":text"       := text s]
    fromIntegral <$> DB.lastInsertRowId c

insertFile :: File -> IO Int
insertFile f = DB.withConnection postsDb $ \c -> do
    DB.executeNamed c   "INSERT INTO Files (Name, Extension, Size, Width, Height) \
                        \VALUES (:name, :ext, :size, :width, :height)" 
                        [ ":name"   := filename f
                        , ":ext"    := (fromEnum $ ext f)
                        , ":size"   := size f
                        , ":width"  := width <$> dim f
                        , ":height" := height <$> dim f]
    fromIntegral <$> DB.lastInsertRowId c

insertPostWithFile :: PostStub -> Int -> IO Int
insertPostWithFile s f = DB.withConnection postsDb $ \c -> do
    DB.executeNamed c   "INSERT INTO Posts (Name, Email, Subject, Text, FileId) \
                        \VALUES (:name, :email, :subject, :text, :fileid)" 
                        [ ":name"       := author s
                        , ":email"      := email s
                        , ":subject"    := subject s
                        , ":text"       := text s
                        , ":fileid"     := f]
    fromIntegral <$> DB.lastInsertRowId c

getFileId :: Text -> IO (Maybe Int)
getFileId name = DB.withConnection postsDb $ \c -> do
    l <- DB.queryNamed c "SELECT Id FROM Files WHERE Name = :name" 
        [":name" := name] :: IO [DB.Only Int]
    return $ DB.fromOnly <$> listToMaybe l

getPosts :: IO [Post]
getPosts = DB.withConnection postsDb $ \c ->
    DB.query_ c "SELECT * FROM posts_and_files" 
