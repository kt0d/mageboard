{-# LANGUAGE OverloadedStrings #-}
module Main where
import Control.Monad
import Control.Monad.Trans

import qualified Network.Wai.Middleware.RequestLogger as Wai (logStdoutDev)
import qualified Network.Wai.Middleware.Static as Wai 
import qualified Web.Scotty as S
import Network.HTTP.Types.Status (notFound404)
import qualified Data.ByteString.Lazy as BS

import Imageboard.Database
import Imageboard.Pages 
import Imageboard.Actions
import Imageboard.Utils
import Graphics.Captcha

-- | Run the server. Routing is defined here.
main :: IO ()
main = do
    setupDb
    S.scotty 3000 $ do
        S.middleware Wai.logStdoutDev
        S.middleware $ Wai.staticPolicy $ Wai.noDots <> Wai.addBase "static"
        S.get   "/captcha.png" $ do
            (sol,cap) <- liftIO $ makeCaptcha
            liftIO $ insertCaptcha sol
            S.raw $ BS.fromStrict cap
        S.get   "/" $
            blaze =<< liftIO (homePage <$> getBoardInfos)
        S.get   "/recent" $
            blaze =<< liftIO (recentView <$> getBoardNames <*> getPosts 100)
        S.get   "/mod"              $ modPage
        S.post  "/login"            $ tryLogin
        S.post  "/logout"           $ logout
        S.get   "/changepass"       $ allowLoggedIn (blaze changePasswordPage)
        S.post  "/changepass"       $ allowLoggedIn changePass
        S.get   "/boardedit/:board" $ allowAdmin $ S.param "board" >>= prepareBoardEdit
        S.post  "/boardedit/:board" $ allowAdmin modifyBoard
        S.get   "/newboard"         $ allowAdmin (blaze createBoardPage)
        S.post  "/newboard"         $ allowAdmin createBoard
        S.get  "/delete-file/:name" $ 
            S.param "name" >>= allowLoggedIn . deleteFile
        S.get "/unlink/:board/:num" $ 
            allowLoggedIn $ join $ unlinkPostFile <$> S.param "board" <*> S.param "num"
        S.get "/delete/:board/:num" $
            allowLoggedIn $ join $deletePost <$> S.param "board" <*> S.param "num"
        S.get "/sticky/:board/:num" $
            allowLoggedIn $ join $ toggleThreadSticky <$> S.param "board" <*> S.param "num"
        S.get "/lock/:board/:num" $
            allowLoggedIn $ join $ toggleThreadLock <$> S.param "board" <*> S.param "num"
        S.get "/autosage/:board/:num" $
            allowLoggedIn $ join $ toggleThreadAutosage <$> S.param "board" <*> S.param "num"
        S.get "/cycle/:board/:num" $
            allowLoggedIn $ join $ toggleThreadCycle <$> S.param "board" <*> S.param "num"
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
        S.post  "/post/:board" $
            S.param "board" >>= createThread
        S.post  "/post/:board/:number" $ do
            join $ createPost <$> S.param "board" <*> S.param "number"
        S.notFound $ do
            S.status notFound404
            blaze $ errorView "404"
