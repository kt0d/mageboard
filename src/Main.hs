{-# LANGUAGE OverloadedStrings #-}
module Main where
import Control.Monad.Trans

import qualified Network.Wai.Middleware.RequestLogger as Wai (logStdoutDev)
import qualified Network.Wai.Middleware.Static as Wai 
import qualified Web.Scotty as S
import Network.HTTP.Types.Status (notFound404)

import Imageboard.Database
import Imageboard.Pages 
import Imageboard.Actions
import Imageboard.Utils

main :: IO ()
main = do
    setupDb
    S.scotty 3000 $ do
        S.middleware Wai.logStdoutDev
        S.middleware $ Wai.staticPolicy $ Wai.noDots <> Wai.addBase "static"
        S.get   "/" $ do
            bs <- liftIO $ getBoardInfos
            blaze $ homePage bs
        S.get   "/recent" $ do
            bs <- liftIO $ getBoardNames
            ps <- liftIO $ getPosts 100
            blaze $ recentView bs ps
        S.get   "/mod"      $ modPage
        S.post  "/login"    $ tryLogin
        S.post  "/logout"   $ logout
        S.get   "/changepass"    $ allowLoggedIn (blaze changePasswordPage)
        S.post  "/changepass"    $ allowLoggedIn changePass
        S.get   "/boardedit/:board" $ allowAdmin $ do
            board <- S.param "board"
            info <- liftIO $ getBoardInfo board
            cs <- liftIO $ getConstraints board
            case (info, cs) of
                (Just i, Just c) -> blaze $ boardModifyPage i c
        S.post  "/boardedit/:board" $ allowAdmin modifyBoard
        S.get   "/newboard" $ allowAdmin (blaze createBoardPage)
        S.post  "/newboard" $ allowAdmin createBoard
        S.get   "/:board" $ do
            board <- S.param "board"
            threads <- liftIO $ getThreads board
            bs <- liftIO $ getBoardNames
            if board `elem` bs
                then blaze $ catalogView bs board threads
                else do
                    S.status notFound404
                    blaze $ errorView "Board does not exist"
        S.get   "/:board/:number" $ do
            board <- S.param "board"
            num <- S.param "number"
            e <- liftIO $ getThread board num
            bs <- liftIO $ getBoardNames 
            case e of
                Nothing -> blaze $ errorView "Thread does not exist"
                Just t -> do
                    S.status notFound404
                    blaze $ threadView bs t
        S.post  "/post/:board" $ do
            board <- S.param "board"
            createThread board
        S.post  "/post/:board/:number" $ do
            num <- S.param "number"
            board <- S.param "board"
            createPost board num
        S.notFound $ do
            S.status notFound404
            blaze $ errorView "404"
