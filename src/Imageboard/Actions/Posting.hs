{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Imageboard.Actions.Posting (
    createPost,
    createThread
) where
import Control.Monad.Except
import Data.Maybe
import Data.Foldable (fold)
import Data.Text (Text)
import qualified Data.Text.Lazy as Lazy
import qualified Data.Text as T
import qualified Web.Scotty as S
import Network.HTTP.Types.Status (created201, badRequest400)
import Imageboard.Database
import Imageboard.Types
import Imageboard.Pages (errorView)
import Imageboard.FileUpload
import Imageboard.Utils

tryMkStub :: BoardConstraints -> Bool -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text 
    -> Either Text PostStub
tryMkStub Constraints{..} hasFile a e s t
    | isLocked  = Left "Board is locked"
    | postText    #< minLen && not hasFile  = Left "Post text too short"
    | postText    #> maxLen                 = Left "Post text too long"
    | postEmail   #> 320                    = Left "Email too long"
    | postSubject #> 128                    = Left "Subject too long"
    | postAuthor  #> 64                     = Left "Author name too long"
    | T.count "\n" postText > maxNewLines   = Left "Too many newlines in post text"
    | otherwise = Right $ Stub postAuthor postEmail postSubject postText
    where
        x #< y = T.compareLength x y == LT
        x #> y = T.compareLength x y == GT
        postAuthor  = case a of
            Just name | not $ T.null name -> name
            _ -> "Nameless"
        postEmail   = fold e
        postSubject = fold s
        postText    = fold t

tryInsertFile :: MonadIO m => FileData -> ExceptT Text m Int
tryInsertFile fdata = do
    (f, fPath) <- liftEither $ tryMkFile fdata
    exists <- liftIO $ getFileId $ filename f
    case exists of
        Just fileId -> return fileId
        Nothing -> do
            newF <- saveFile f fdata fPath
            liftIO $ insertFile newF

tryInsertThread :: MonadIO m => Board -> PostStub -> Maybe FileData -> ExceptT Text m ()
tryInsertThread b stub mdata = case mdata of
    Just fdata -> do
        fileId <- tryInsertFile fdata
        liftIO $ insertThreadWithFile b stub fileId
    Nothing -> 
        liftIO $ insertThread b stub 

tryInsertPost :: MonadIO m => Board -> Int -> PostStub -> Maybe FileData -> ExceptT Text m ()
tryInsertPost b p stub mdata = case mdata of
    Just fdata -> do
        fileId <- tryInsertFile fdata
        liftIO $ insertPostWithFile b p stub fileId
    Nothing -> 
        liftIO $ insertPost b p stub 

validatePost :: Board -> Bool -> S.ActionM (Either Text PostStub)
validatePost b hasFile = do
    cs <- liftIO $ getConstraints b
    case cs of
        Nothing -> return $ Left "Board does not exist"
        Just x -> tryMkStub x hasFile
            <$> maybeParam "name"
            <*> maybeParam "email"
            <*> maybeParam "subject"
            <*> maybeParam "comment"

-- | This action will try to insert post sent by the user.
-- In case of failure it will send user appropriate error page.
createPost :: Board -> Int -> S.ActionM ()
createPost b p = do 
    postFile <- maybeFile
    stubErr <- validatePost b $ isJust postFile
    result <- runExceptT $ do 
        stub <- liftEither $ stubErr
        exists <- liftIO $ checkThread b p
        if exists 
            then tryInsertPost b p stub postFile
            else throwError "Thread does not exist"
    case result of
        Left msg -> do
            S.status badRequest400
            blaze $ errorView msg
        Right _ -> do
            S.status created201
            S.redirect $ "/" <> Lazy.fromStrict b <> "/" <> (Lazy.pack $ show p)

createThread :: Board -> S.ActionM ()
createThread b = do
    postFile <- maybeFile
    stubErr <- validatePost b $ isJust postFile
    result <- runExceptT $ do 
        stub <- liftEither $ stubErr
        tryInsertThread b stub postFile
    case result of
        Left msg -> do
            S.status badRequest400
            blaze $ errorView msg
        Right _ -> do
            S.status created201
            S.redirect $ "/" <> Lazy.fromStrict b

