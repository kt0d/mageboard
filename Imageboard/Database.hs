{-# LANGUAGE OverloadedStrings #-}
module Imageboard.Database (
    setupDb,
    insertPost,
    getPosts
) where
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Database.SQLite.Simple as DB
import qualified Database.SQLite.Simple.FromRow as DB (fieldWith)
import qualified Database.SQLite.Simple.FromField as DB (FieldParser, fieldData, returnError)
import qualified Database.SQLite.Simple.Ok as DB (Ok(..))
import qualified Database.SQLite.Simple.Time.Implementation as DB
import qualified Database.SQLite3 as DirectDB (open, close, exec)
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)

import Imageboard.Types

postsDb :: FilePath
postsDb = "posts.db" 

schemaFile :: FilePath
schemaFile = "schema.sql"

instance DB.FromRow Post where
    fromRow = Post  <$> DB.field 
                    <*> DB.fieldWith parseDate 
                    <*> (Stub <$> DB.field <*> DB.field <*> DB.field <*> DB.field)
        where 
            parseDate :: DB.FieldParser UTCTime
            parseDate f = case DB.fieldData f of 
                -- INTEGER as Unix Time, the number of seconds since 1970-01-01 00:00:00 UTC. 
                DB.SQLInteger x -> DB.Ok . posixSecondsToUTCTime $ fromIntegral x
                -- TEXT as ISO8601 strings ("YYYY-MM-DD HH:MM:SS.SSS"). 
                DB.SQLText x -> case DB.parseUTCTime x of 
                    Left s -> DB.returnError DB.ConversionFailed f s
                    Right t -> DB.Ok $ t
                _ -> DB.returnError DB.Incompatible f "expecting an SQLInteger column type"

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
                        [ ":name"     DB.:= author s
                        , ":email"    DB.:= email s
                        , ":subject"  DB.:= subject s
                        , ":text"     DB.:= text s]
    fromIntegral <$> DB.lastInsertRowId c
    

getPosts :: IO [Post]
getPosts = DB.withConnection postsDb $ \c ->
    DB.query_ c "SELECT * FROM Posts ORDER BY Number DESC" 
