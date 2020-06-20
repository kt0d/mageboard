
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Imageboard.Actions.Moderation (
    deleteFile,
    unlinkPostFile,
    deletePost
) where
import Control.Monad.Except
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
