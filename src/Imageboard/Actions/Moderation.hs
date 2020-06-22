
{-# LANGUAGE OverloadedStrings, BlockArguments #-}
module Imageboard.Actions.Moderation (
    deleteFile,
    unlinkPostFile,
    deletePost,
    toggleThreadSticky,
    toggleThreadAutosage,
    toggleThreadLock,
    toggleThreadCycle
) where
import Control.Monad.Except
import qualified Data.Text.Lazy as Lazy
import Data.Text (Text)
import qualified Web.Scotty as S
import Network.HTTP.Types.Status (badRequest400)
import Imageboard.Pages (errorView)
import Imageboard.Utils
import Imageboard.Database
import Imageboard.Types (Board)

checkIfPostExists :: Board -> Int -> S.ActionM ()
checkIfPostExists b n = do
    exist <- liftIO $ checkThread b n
    when (not exist) $ do
        S.status badRequest400
        blaze $ errorView "Post does not exist"
        S.finish

doActionOnPost :: (Board -> Int -> IO ()) -> Board -> Int -> S.ActionM ()
doActionOnPost action b n = do
    checkIfPostExists b n
    liftIO $ action b n
    returnToThread b n

returnToThread :: Board -> Int -> S.ActionM ()
returnToThread b n = S.redirect $ "/" <> Lazy.fromStrict b <> "/" <> (Lazy.pack $ show n)

-- | Delete file.
deleteFile :: Text -> S.ActionM ()
deleteFile f = do
    fileId <- liftIO $ getFileId f
    case fileId of
        Nothing -> do
            S.status badRequest400
            blaze $ errorView "Post does not exist"
        _ -> do
            liftIO $ removeFile f
            blaze $ errorView "File removed"

-- | Remove file from post.
unlinkPostFile :: Board -> Int -> S.ActionM ()
unlinkPostFile b n = do
    checkIfPostExists b n
    liftIO $ unlinkFile b n 
    blaze $ errorView "File unlinked from post"

-- | Delete post.
deletePost :: Board -> Int -> S.ActionM ()
deletePost b n = do
    checkIfPostExists b n
    liftIO $ removePost b n
    blaze $ errorView "Post removed"

toggleThreadSticky :: Board -> Int -> S.ActionM ()
toggleThreadSticky = doActionOnPost toggleSticky

toggleThreadAutosage ::  Board -> Int -> S.ActionM ()
toggleThreadAutosage = doActionOnPost toggleAutosage

toggleThreadLock ::  Board -> Int -> S.ActionM ()
toggleThreadLock = doActionOnPost toggleLock 

toggleThreadCycle ::  Board -> Int -> S.ActionM ()
toggleThreadCycle = doActionOnPost toggleCycle 