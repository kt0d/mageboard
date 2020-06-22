{-# LANGUAGE OverloadedStrings, RecordWildCards, BlockArguments #-}
module Imageboard.Actions.Display (
    displayCatalog,
    displayThread,
    displayCaptcha
) where
import Control.Monad.Except
import Graphics.Captcha (makeCaptcha)
import qualified Data.ByteString.Lazy as BS
import qualified Web.Scotty as S
import Network.HTTP.Types.Status (notFound404)
import Imageboard.Pages (errorView, catalogView, threadView)
import Imageboard.Utils
import Imageboard.Database
import Imageboard.Types (Board)

displayCaptcha :: S.ActionM ()
displayCaptcha = do
    (sol,cap) <- liftIO $ makeCaptcha
    liftIO $ insertCaptcha sol
    S.raw $ BS.fromStrict cap

displayCatalog :: Board -> S.ActionM ()
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

displayThread :: Board -> Int -> S.ActionM ()
displayThread b n = do
    liftIO $ liftM3 (,,) <$> getBoardInfo b <*> getConstraints b <*> getThread b n
    >>= maybe
        do
            S.status notFound404
            blaze $ errorView "Thread does not exist"
        \(i,c,t) -> do
            bs <- liftIO $ getBoardNames 
            blaze $ threadView i c bs t
