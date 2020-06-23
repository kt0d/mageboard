{-# LANGUAGE OverloadedStrings #-}
module Main where
import Control.Monad
import Control.Monad.Trans

import qualified Network.Wai.Middleware.RequestLogger as Wai (logStdoutDev)
import qualified Network.Wai.Middleware.Static as Wai 
import qualified Web.Scotty as S
import Network.HTTP.Types.Status (notFound404)

import Imageboard.Config (setupDirs)
import Imageboard.Database
import Imageboard.Pages 
import Imageboard.Actions
import Imageboard.Utils

-- | Run the server. Routing is defined here.
main :: IO ()
main = do
    setupDb
    S.scotty 3000 $ do
        S.middleware Wai.logStdoutDev
        S.middleware $ Wai.staticPolicy $ Wai.noDots <> Wai.addBase "static"
        -- Captcha
        S.get   "/captcha.png" $ displayCaptcha
        S.get   "/" $
            blaze =<< liftIO (homePage <$> getBoardInfos)
        S.get   "/recent" $
            blaze =<< liftIO (recentView <$> getBoardNames <*> getPosts 100)
        -- Account actions
        S.get   "/mod"              $ modPage
        S.post  "/login"            $ tryLogin
        S.post  "/logout"           $ logout
        S.get   "/changepass"       $ allowLoggedIn (blaze changePasswordPage)
        S.post  "/changepass"       $ allowLoggedIn changePass
        -- Admin actions
        S.get   "/create-account"   $ allowAdmin $ blaze createAccountPage
        S.post  "/create-account"   $ allowAdmin createAccount
        S.get   "/boardedit/:board" $ allowAdmin $ S.param "board" >>= prepareBoardEdit
        S.post  "/boardedit/:board" $ allowAdmin modifyBoard
        S.get   "/newboard"         $ allowAdmin (blaze createBoardPage)
        S.post  "/newboard"         $ allowAdmin createBoard
        -- Moderation actions
        S.get  "/delete-file/:name" $ 
            S.param "name" >>= allowLoggedIn . deleteFile
        S.get "/unlink/:board/:num" $ 
            allowLoggedIn $ join $ unlinkPostFile <$> S.param "board" <*> S.param "num"
        S.get "/delete/:board/:num" $
            allowLoggedIn $ join $ deletePost <$> S.param "board" <*> S.param "num"
        S.get "/sticky/:board/:num" $
            allowLoggedIn $ join $ toggleThreadSticky <$> S.param "board" <*> S.param "num"
        S.get "/lock/:board/:num" $
            allowLoggedIn $ join $ toggleThreadLock <$> S.param "board" <*> S.param "num"
        S.get "/autosage/:board/:num" $
            allowLoggedIn $ join $ toggleThreadAutosage <$> S.param "board" <*> S.param "num"
        S.get "/cycle/:board/:num" $
            allowLoggedIn $ join $ toggleThreadCycle <$> S.param "board" <*> S.param "num"
        -- Browsing boards and threads
        S.get   "/:board"       $ S.param "board" >>= displayCatalog
        S.get   "/:board/:num"  $ join $ displayThread <$> S.param "board" <*> S.param "num"
        -- Posting
        S.post  "/post/:board"  $ S.param "board" >>= createThread
        S.post  "/post/:board/:number" $ 
            join $ createPost <$> S.param "board" <*> S.param "number"
        S.notFound $ do
            S.status notFound404
            blaze $ errorView "404"
