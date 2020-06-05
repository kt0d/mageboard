{-# LANGUAGE OverloadedStrings #-}
module Main where
import Control.Monad.Trans

import qualified Network.Wai.Middleware.RequestLogger as Wai (logStdoutDev)
import qualified Network.Wai.Middleware.Static as Wai 
import qualified Web.Scotty as S

import Imageboard.Database (setupDb, getPosts)
import Imageboard.Pages (boardView)
import Imageboard.Actions


main :: IO ()
main = do
    setupDb
    S.scotty 3000 $ do
        S.middleware Wai.logStdoutDev
        S.middleware $ Wai.staticPolicy $ Wai.noDots <> Wai.addBase "static"
        S.get "/" $ do
            posts <- liftIO getPosts
            blaze $ boardView posts
        S.post "/post" createPost
