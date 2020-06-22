{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings, TypeOperators, RecordWildCards #-}
module Imageboard.Database (
    setupDb,
    -- * Post and thread insertion
    insertPost,
    insertFile,
    insertPostWithFile,
    insertThread,
    insertThreadWithFile,
    -- * Post and thread queries
    getPosts,
    getThread,
    checkThread,
    getThreads,
    getThreadInfo,
    getFileId,
    -- * Board queries and management
    getConstraints,
    getBoardNames,
    getBoardInfo,
    getBoardInfos,
    updateBoard,
    insertBoard,
    -- * Account management
    getPasswordHash,
    insertSessionToken,
    removeSessionToken,
    getAccountByToken,
    checkSession,
    insertAccount,
    changePassword,
    -- * Moderation
    removeFile,
    removePost,
    unlinkFile,
    -- * Toggle one of ThreadFlags
    toggleSticky,
    toggleLock,
    toggleAutosage,
    toggleCycle
) where
import Control.Monad (liftM2, liftM4)
import Data.Text (Text)
import Data.Maybe (listToMaybe)
import Data.Functor ((<&>))
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
import Imageboard.Types
import qualified Imageboard.Config as Config

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

instance DB.FromRow ThreadInfo where
    fromRow = ThreadInfo 
                <$> DB.fieldWith parseDate
                <*> (Flags 
                    <$> DB.field
                    <*> DB.field 
                    <*> DB.field
                    <*> DB.field)
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

instance DB.FromRow AccountInfo where
    fromRow = AccountInfo 
                <$> DB.field 
                <*> DB.fieldWith parseDate
                <*> (toEnum <$> DB.field) 

-- | Create sqlite3 database file using schema file.
setupDb :: IO ()
setupDb = do
    conn <- DirectDB.open Config.postsDb
    schema <- T.readFile Config.schemaFile
    DirectDB.exec conn schema
    -- Insert default admin account.
    hash <- P.hashPassword "password"
    DB.executeNamed (DB.Connection conn)   
        "INSERT OR IGNORE INTO Accounts (Username, Password, Role) \
        \VALUES (:user, :hash, 0)"
        [ ":user" := ("admin" :: Text)
        , ":hash" := unPasswordHash hash] 
    DirectDB.close conn

runDb :: (DB.Connection -> IO a) -> IO a 
runDb = DB.withConnection Config.postsDb

foreignKeyPragma :: DB.Connection -> IO ()
foreignKeyPragma c = DB.execute_ c   "PRAGMA foreign_keys=1"

-- | Insert file information into database.
insertFile :: File -> IO Int -- ^ File Id.
insertFile f = runDb $ \c -> do
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
insertPost b p s = runDb $ \c -> DB.withTransaction c $
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
insertPostWithFile b p s f = runDb $ \c -> DB.withTransaction c $
    DB.executeNamed c   "INSERT INTO Posts (Name, Email, Subject, Text, FileId, Parent, Board) \
                        \VALUES (:name, :email, :subject, :text, :fileid, :parent, :board)" 
                        [ ":name"       := author s
                        , ":email"      := email s
                        , ":subject"    := subject s
                        , ":text"       := text s
                        , ":fileid"     := f
                        , ":parent"     := p
                        , ":board"      := b]

-- | Insert new thread with no file attached.
insertThread :: Board -> PostStub -> IO ()
insertThread b s = runDb $ \c -> DB.withTransaction c $ do
    DB.executeNamed c   "INSERT INTO Posts (Name, Email, Subject, Text, Board, \
                        \LastBump, Sticky, Lock, Autosage, Cycle, ReplyCount) \
                        \VALUES (:name, :email, :subject, :text, :board, \
                        \0, 0, 0, 0, 0, 0)" 
                        [ ":name"       := author s
                        , ":email"      := email s
                        , ":subject"    := subject s
                        , ":text"       := text s
                        , ":board"      := b]

-- | Insert post with file attached, using previously obtained file Id.
insertThreadWithFile :: Board -> PostStub -> Int -> IO ()
insertThreadWithFile b s f = runDb $ \c -> DB.withTransaction c $
    DB.executeNamed c   "INSERT INTO Posts (Name, Email, Subject, Text, FileId, Board, \
                        \LastBump, Sticky, Lock, Autosage, Cycle, ReplyCount) \
                        \VALUES (:name, :email, :subject, :text, :fileid, :board, \
                        \0, 0, 0, 0, 0, 0)"  
                        [ ":name"       := author s
                        , ":email"      := email s
                        , ":subject"    := subject s
                        , ":text"       := text s
                        , ":fileid"     := f
                        , ":board"      := b]

-- | Obtain ID of previously inserted file.
getFileId :: Text -- ^ Filename
    -> IO (Maybe Int) -- ^ File Id, if given filename exists in database.
getFileId f = runDb $ \c -> do
    DB.queryNamed c "SELECT Id FROM Files WHERE Name = :name" 
                    [":name" := f] :: IO [DB.Only Int]
    <&> listToMaybe <&> fmap DB.fromOnly

-- | 'getPosts' @n@ returns @n@ most recent posts, from all boards.
getPosts :: Int -> IO [Post]
getPosts n = runDb $ \c -> do
    DB.queryNamed c "SELECT * FROM posts_and_files ORDER BY Date DESC LIMIT :n" 
                    [":n" := n]

-- | Get thread by board name and number.
getThread :: Board -> Int -- ^ Post number.
         -> IO (Maybe Thread)
getThread b n = runDb $ \c -> do
    h <-  DB.queryNamed c   "SELECT * FROM threads WHERE Number = :number AND Board = :board" 
                            [":number" := n, ":board" := b]
    ps <- DB.queryNamed c   "SELECT * FROM posts_and_files WHERE Parent = :number \
                            \AND Board = :board ORDER BY Number ASC"
                            [":number" := n, ":board" := b] 
    case h of
        [] -> return $ Nothing
        x:_ -> return $ Just $ Thread (mkThreadHead x) ps

-- | Check if thread with given number exists on given board. 
checkThread :: Board -> Int -> IO Bool
checkThread b n = runDb $ \c -> do
    DB.queryNamed c "SELECT EXISTS\
                    \(SELECT 1 FROM threads WHERE Number = :number AND Board = :board)" 
                    [":number" := n, ":board" := b] :: IO [DB.Only Bool]
    <&> any DB.fromOnly

-- | Get thread information by board name and number. 
getThreadInfo :: Board -> Int -> IO (Maybe ThreadInfo)
getThreadInfo b n = runDb $ \c -> do
    DB.queryNamed c "SELECT LastBump, Sticky, Lock, Autosage, Cycle, ReplyCount \
                    \FROM threads WHERE Number = :number AND Board = :board" 
                    [":number" := n, ":board" := b]
    <&> listToMaybe

-- | Get all threads from given board.
getThreads :: Board -> IO [ThreadHead]
getThreads b = runDb $ \c -> do
    DB.queryNamed c "SELECT * FROM threads WHERE Board = :board \
                    \ORDER BY Sticky DESC, LastBump DESC" 
                    [":board" := b]
    <&> map mkThreadHead

-- | Get constraints of given board, if it exists.
getConstraints :: Board -> IO (Maybe BoardConstraints)
getConstraints b = runDb $ \c -> do
    DB.queryNamed c "SELECT Lock, PostMinLength, PostMaxLength, PostMaxNewlines, \
                    \PostLimit, ThreadLimit FROM Boards WHERE Name = :board"
                    [":board" := b] :: IO [BoardConstraints]
    <&> listToMaybe

-- | Get names of all existing boards.
getBoardNames :: IO [Board]
getBoardNames = runDb $ \c -> do
    DB.query_ c "SELECT Name From Boards" <&> map DB.fromOnly

-- | Get more detailed information for all existing boards.
getBoardInfos :: IO [BoardInfo]
getBoardInfos = runDb $ \c -> do
    DB.query_ c "SELECT Name, Title, Subtitle From Boards"

-- | Get more detailed information for given board.
getBoardInfo :: Board -> IO (Maybe BoardInfo)
getBoardInfo b = runDb $ \c -> do
    DB.queryNamed c "SELECT Name, Title, Subtitle FROM Boards WHERE Name = :board"
                    [":board" := b] :: IO [BoardInfo]
    <&> listToMaybe

-- | Insert new account into database.
insertAccount :: Username -> PasswordHash Bcrypt -> Role -> IO ()
insertAccount u h r = runDb $ \c -> do
    DB.executeNamed c   "INSERT INTO Accounts (Username, Password, Role) \
                        \VALUES (:name, :hash, :role)" 
                        [ ":name"   := u
                        , ":hash"   := unPasswordHash h
                        , ":role"   := fromEnum r]

-- | Change password of given user.
changePassword :: Username -> PasswordHash Bcrypt -> IO ()
changePassword u h = runDb $ \c -> do
    DB.executeNamed c   "UPDATE Accounts SET Password = :hash WHERE Username = :name" 
                        [ ":name"   := u
                        , ":hash"   := unPasswordHash h]
 
-- | Get hashed password if given user exists.
getPasswordHash :: Username -> IO (Maybe (PasswordHash Bcrypt))
getPasswordHash u =  runDb $ \c -> do
    q <- DB.queryNamed c    "SELECT Password From Accounts WHERE USERNAME = :user"
                            [":user" := u] :: IO [DB.Only Text]
    return $ PasswordHash <$> DB.fromOnly <$> listToMaybe q

-- | Insert new sesion token for given user. Will delete old session tokens for the user.
insertSessionToken :: Username -> SessionKey -> IO ()
insertSessionToken a t = runDb $ \c -> do
    DB.executeNamed c   "INSERT INTO Sessions (Username, Key) \
                        \VALUES (:name, :token)" 
                        [ ":name"   := a
                        , ":token"  := t]

-- | Remove given session token from database.
removeSessionToken :: SessionKey -> IO ()
removeSessionToken t = runDb $ \c -> do
    DB.executeNamed c   "DELETE FROM Sessions WHERE Username = \
                        \(SELECT Username FROM Sessions WHERE Key = :token)" 
                        [ ":token"  := t]

-- | Check if session token is still valid.
checkSession :: SessionKey -> IO Bool
checkSession t = runDb $ \c -> do
    DB.queryNamed c "SELECT EXISTS(SELECT * FROM Sessions WHERE Key = :token \
                    \AND ExpireDate > STRFTIME('%s', 'now'))"
                    [":token" := t]  :: IO [DB.Only Bool]
    <&> any DB.fromOnly

-- | Get account associated with given session token, if it exists.
getAccountByToken :: SessionKey -> IO (Maybe AccountInfo)
getAccountByToken t = runDb $ \c -> do
    DB.queryNamed c "SELECT Accounts.Username, CreationDate, Role \
                    \FROM Accounts JOIN Sessions ON Accounts.Username = Sessions.Username \
                    \WHERE Sessions.Key = :token AND ExpireDate > STRFTIME('%s', 'now')"
                    [":token" := t]
    <&> listToMaybe

-- | 
updateBoard :: Board -- ^ Old board name.
            -> BoardInfo -> BoardConstraints -> IO ()
updateBoard b BoardInfo{..} Constraints{..} = runDb $ \c -> do
    foreignKeyPragma c
    DB.execute c    "UPDATE Boards SET Name = ?, Title = ?, Subtitle = ?, \
                    \Lock = ?, PostMinLength = ?, PostMaxLength = ?, \
                    \PostMaxNewlines = ?, PostLimit = ?, ThreadLimit = ? \
                    \WHERE Name = ?" 
                    (name, title, subtitle,
                    isLocked, minLen, maxLen,
                    maxNewLines, maxReplies, maxThreads, b)

-- | Insert new board into a database.
insertBoard :: BoardInfo -> BoardConstraints -> IO ()
insertBoard BoardInfo{..} Constraints{..} = runDb $ \c -> do
    DB.execute c    "INSERT INTO Boards (Name, Title, Subtitle, \
                    \Lock, PostMinLength, PostMaxLength, PostMaxNewLines, PostLimit, ThreadLimit) \
                    \VALUES (?,?,?,?,?,?,?,?,?)"
                    (name, title, subtitle,
                    isLocked, minLen, maxLen, maxNewLines, maxReplies, maxThreads)
    
-- | Remove file with given filename from database.
removeFile :: Text -> IO ()
removeFile f = runDb $ \c -> do
    foreignKeyPragma c
    DB.executeNamed c   "DELETE FROM Files WHERE Name = :name" 
                        [ ":name"  := f]

-- | Remove posts with given number from given board.
removePost :: Board -> Int -> IO ()
removePost b n = runDb $ \c -> do
    foreignKeyPragma c
    DB.executeNamed c   "DELETE FROM Posts WHERE Board = :board AND Number = :num" 
                        [ ":board"  := b, ":num" := n]

-- | Unlink file from post with given number on given board, 
-- a.k.a. turn it into post with no file attached, without removing
-- file from database.
unlinkFile :: Board -> Int -> IO ()
unlinkFile b n = runDb $ \c -> do
    DB.executeNamed c   "UPDATE Posts SET FileId = NULL WHERE Board = :board AND Number = :num" 
                        [ ":board"  := b, ":num" := n]

execOnThread :: DB.Query -> Board -> Int -> IO ()
execOnThread q b n = runDb $ \c -> DB.execute c q (b,n)

toggleSticky :: Board -> Int -> IO ()
toggleSticky = execOnThread "UPDATE Posts SET Sticky = NOT STICKY WHERE Board = ? AND Number = ?" 

toggleAutosage :: Board -> Int -> IO ()
toggleAutosage = execOnThread "UPDATE Posts SET Autosage = NOT Autosage WHERE Board = ? AND Number = ?" 

toggleLock :: Board -> Int -> IO ()
toggleLock = execOnThread "UPDATE Posts SET Lock = NOT Lock WHERE Board = ? AND Number = ?" 

toggleCycle :: Board -> Int -> IO ()
toggleCycle = execOnThread "UPDATE Posts SET Cycle = NOT Cycle WHERE Board = ? AND Number = ?"