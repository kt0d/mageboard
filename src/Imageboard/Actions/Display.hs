{-# LANGUAGE OverloadedStrings, RecordWildCards, BlockArguments #-}
module Imageboard.Actions.Display (
    displayCatalog,
    displayThread,
    displayCaptcha,
    displayRecent,
    displayHomePage
) where
import Control.Monad.Except
import Graphics.Captcha (makeCaptcha)
import qualified Data.ByteString.Lazy as BS
import qualified Web.Scotty as S
import Network.HTTP.Types.Status (notFound404)
import Imageboard.Pages (errorView, catalogView, threadView, homePage, recentView)
import Imageboard.Actions.Common
import Imageboard.Database
import Imageboard.Types (Board)

-- | Create new captcha.
displayCaptcha :: Action
displayCaptcha = do
    (sol,cap) <- liftIO $ makeCaptcha
    liftIO $ insertCaptcha sol
    S.raw $ BS.fromStrict cap

-- | Display all threads on board in catalog style.
displayCatalog :: Board -> Action
displayCatalog b = do
    liftIO $ liftM2 (,) <$> getBoardInfo b <*> getConstraints b
    >>= maybe
        do
            S.status notFound404
            blaze $ errorView "Board does not exist"
        \(i,c) -> do
            bs <- liftIO $ getBoardNames
            threads <- liftIO $ getThreads b
            blaze $ catalogView i c bs threads

-- | Display single thread with replies.
displayThread :: Board -> Int -> Action
displayThread b n = do
    liftIO $ liftM3 (,,) <$> getBoardInfo b <*> getConstraints b <*> getThread b n
    >>= maybe
        do
            S.status notFound404
            blaze $ errorView "Thread does not exist"
        \(i,c,t) -> do
            bs <- liftIO $ getBoardNames 
            blaze $ threadView i c bs t

-- | Display 100 most recent posts.
displayRecent :: Action
displayRecent = blaze =<< liftIO (recentView <$> getBoardNames <*> getPosts 100)

displayHomePage :: Action
displayHomePage = blaze =<< liftIO (homePage <$> getBoardInfos)