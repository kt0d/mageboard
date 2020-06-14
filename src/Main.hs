{-# LANGUAGE OverloadedStrings #-}
module Main where
import Control.Monad.Trans

import qualified Network.Wai.Middleware.RequestLogger as Wai (logStdoutDev)
import qualified Network.Wai.Middleware.Static as Wai 
import qualified Web.Scotty as S
import Network.HTTP.Types.Status (notFound404)

import Imageboard.Database (setupDb, getPosts, getThreads, getThread, getBoardNames)
import Imageboard.Pages (catalogView, threadView, errorView)
import Imageboard.Actions


main :: IO ()
main = do
    setupDb
    S.scotty 3000 $ do
        S.middleware Wai.logStdoutDev
        S.middleware $ Wai.staticPolicy $ Wai.noDots <> Wai.addBase "static"
        S.get "/:board" $ do
            board <- S.param "board"
            threads <- liftIO $ getThreads board
            bs <- liftIO $ getBoardNames
            blaze $ catalogView board threads
        S.get "/:board/:number" $ do
            board <- S.param "board"
            num <- S.param "number"
            e <- liftIO $ getThread board num
            bs <- liftIO $ getBoardNames 
            blaze $ case e of
                Nothing -> errorView "Thread does not exist"
                Just t -> threadView t
        S.post "/post/:board" $ do
            board <- S.param "board"
            createThread board
        S.post "/post/:board/:number" $ do
            num <- S.param "number"
            board <- S.param "board"
            createPost board num
        S.notFound $ do
            S.status notFound404
            blaze $ errorView "404"
