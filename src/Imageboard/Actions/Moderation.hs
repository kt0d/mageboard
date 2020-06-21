
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
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
import Network.HTTP.Types.Status (created201, badRequest400)
import Imageboard.Pages (errorView)
import Imageboard.Utils
import Imageboard.Database
import Imageboard.Types (Board)

-- | Delete file.
deleteFile :: Text -> S.ActionM ()
deleteFile f = do
    liftIO $ removeFile f
    blaze $ errorView "File removed"

-- | Remove file from post.
unlinkPostFile :: Board -> Int -> S.ActionM ()
unlinkPostFile b n = do
    liftIO $ unlinkFile b n 
    blaze $ errorView "File unlinked from post"

-- | Delete post.
deletePost :: Board -> Int -> S.ActionM ()
deletePost b n = do
    liftIO $ removePost b n
    blaze $ errorView "Post removed"

returnToThread :: Board -> Int -> S.ActionM ()
returnToThread b n = S.redirect $ "/" <> Lazy.fromStrict b <> "/" <> (Lazy.pack $ show n)

toggleThreadSticky :: Board -> Int -> S.ActionM ()
toggleThreadSticky b n = do
    liftIO $ toggleSticky b n
    returnToThread b n

toggleThreadAutosage ::  Board -> Int -> S.ActionM ()
toggleThreadAutosage b n = do
    liftIO $ toggleAutosage b n
    returnToThread b n

toggleThreadLock ::  Board -> Int -> S.ActionM ()
toggleThreadLock b n = do
    liftIO $ toggleLock b n
    returnToThread b n

toggleThreadCycle ::  Board -> Int -> S.ActionM ()
toggleThreadCycle b n = do
    liftIO $ toggleCycle b n
    returnToThread b n