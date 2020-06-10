{-# LANGUAGE OverloadedStrings #-}
module Main where
import Control.Monad.Trans

import qualified Network.Wai.Middleware.RequestLogger as Wai (logStdoutDev)
import qualified Network.Wai.Middleware.Static as Wai 
import qualified Web.Scotty as S

import Imageboard.Database (setupDb, getPosts, getThreads, getThread)
import Imageboard.Pages (boardView, threadView, errorView)
import Imageboard.Actions


main :: IO ()
main = do
    setupDb
    S.scotty 3000 $ do
        S.middleware Wai.logStdoutDev
        S.middleware $ Wai.staticPolicy $ Wai.noDots <> Wai.addBase "static"
        S.get "/" $ do
            threads <- liftIO getThreads
            blaze $ boardView threads
        S.get "/:number" $ do
            num <- S.param "number"
            e <- liftIO $ getThread num
            blaze $ case e of
                Left msg -> errorView msg
                Right t -> threadView t
        S.post "/post/:number" $ do
            num <- S.param "number"
            createPost $ Just num
        S.post "/post" $ do
            createPost Nothing
