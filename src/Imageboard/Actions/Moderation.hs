
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
import Imageboard.Actions.Common
import Imageboard.Database
import Imageboard.Types (Board)

checkIfPostExists :: Board -> Int -> Action
checkIfPostExists b n = do
    exist <- liftIO $ checkThread b n
    when (not exist) $ do
        S.status badRequest400
        blaze $ errorView "Post does not exist"
        S.finish

doActionOnPost :: (Board -> Int -> IO ()) -> Board -> Int -> Action
doActionOnPost action b n = do
    checkIfPostExists b n
    liftIO $ action b n
    returnToThread b n

returnToThread :: Board -> Int -> Action
returnToThread b n = S.redirect $ "/" <> Lazy.fromStrict b <> "/" <> (Lazy.pack $ show n)

-- | Delete file.
deleteFile :: Text -> Action
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
unlinkPostFile :: Board -> Int -> Action
unlinkPostFile b n = do
    checkIfPostExists b n
    liftIO $ unlinkFile b n 
    blaze $ errorView "File unlinked from post"

-- | Delete post.
deletePost :: Board -> Int -> Action
deletePost b n = do
    checkIfPostExists b n
    liftIO $ removePost b n
    blaze $ errorView "Post removed"

toggleThreadSticky :: Board -> Int -> Action
toggleThreadSticky = doActionOnPost toggleSticky

toggleThreadAutosage ::  Board -> Int -> Action
toggleThreadAutosage = doActionOnPost toggleAutosage

toggleThreadLock ::  Board -> Int -> Action
toggleThreadLock = doActionOnPost toggleLock 

toggleThreadCycle ::  Board -> Int -> Action
toggleThreadCycle = doActionOnPost toggleCycle 