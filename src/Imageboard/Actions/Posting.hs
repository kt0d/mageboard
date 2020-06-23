{-# LANGUAGE OverloadedStrings, RecordWildCards, BlockArguments #-}
module Imageboard.Actions.Posting (
    createPost,
    createThread
) where
import Control.Monad.Except
import Data.Maybe
import Data.String (IsString)
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
import Imageboard.Actions.Common

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
    exists <-liftIO $ getFileId $ filename f
    maybe
        do
            newF <- saveFile f fdata fPath
            liftIO $ insertFile newF
        return
        exists

canReply :: ThreadInfo -> BoardConstraints -> Either Text ()
canReply ThreadInfo{..} Constraints{..}
    | lock flags    = Left "Thread is locked"
    | (not $ cycle_ flags) && (replyCount == maxReplies) = Left "Thread reached maximum number of replies"
    | otherwise = Right ()

tryInsertThread :: MonadIO m => Board -> PostStub -> Maybe FileData -> ExceptT Text m ()
tryInsertThread b stub = maybe
    do liftIO $ insertThread b stub 
    \fdata -> do
        fileId <- tryInsertFile fdata
        liftIO $ insertThreadWithFile b stub fileId
    

tryInsertPost :: MonadIO m => Board -> Int -> PostStub -> Maybe FileData -> ExceptT Text m ()
tryInsertPost b p stub = maybe
    do liftIO $ insertPost b p stub 
    \fdata -> do
        fileId <- tryInsertFile fdata
        liftIO $ insertPostWithFile b p stub fileId
        

validatePost :: BoardConstraints -> Bool -> S.ActionM (Either Text PostStub)
validatePost cstr hasFile = do
    tryMkStub cstr hasFile
            <$> maybeParam "name"
            <*> maybeParam "email"
            <*> maybeParam "subject"
            <*> maybeParam "comment"

validateCaptcha :: (IsString a, MonadIO m, MonadError a m) => Text -> m ()
validateCaptcha captcha = do
    validCaptcha <- liftIO $ checkCaptcha captcha
    when (not validCaptcha) $ throwError "Invalid or expired captcha"

-- | This action will try to insert post sent by the user.
-- In case of failure it will send user appropriate error page.
createPost :: Board -> Int -> Action
createPost b p = do 
    captcha <- S.param "captcha"
    postFile <- maybeFile
    result <- runExceptT $ do 
        validateCaptcha captcha
        cstr <- maybetoExcept "Board does not exist" =<< (liftIO $ getConstraints b)
        stub <- liftEither =<< (lift $ validatePost cstr $ isJust postFile)
        info <- maybetoExcept "Thread does not exist" =<< (liftIO $ getThreadInfo b p)
        liftEither $ canReply info cstr
        tryInsertPost b p stub postFile
    case result of
        Left msg -> do
            S.status badRequest400
            blaze $ errorView msg
        Right _ -> do
            S.status created201
            S.redirect $ "/" <> Lazy.fromStrict b <> "/" <> (Lazy.pack $ show p)

-- | This action will try to insert new thread sent by the user.
-- In case of failure it will send user appropriate error page.
createThread :: Board -> Action
createThread b = do
    postFile <- maybeFile
    captcha <- S.param "captcha"
    runExceptT do 
        validateCaptcha captcha
        cstr <- maybetoExcept "Board does not exist" =<< (liftIO $ getConstraints b)
        stub <- liftEither =<< (lift $ validatePost cstr $ isJust postFile)
        tryInsertThread b stub postFile
    >>= either
        (\msg -> do
            S.status badRequest400
            blaze $ errorView msg)
        (\_ -> do
            S.status created201
            S.redirect $ "/" <> Lazy.fromStrict b)
