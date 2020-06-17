{-# LANGUAGE OverloadedStrings #-}
module Imageboard.Auth (
    modPage,
    tryLogin
) where
import Control.Monad.IO.Class
import qualified Data.ByteString.Base64 as Base64
import Data.Text (Text)
import qualified Data.Text.Encoding as TE 
import qualified Data.Text.Lazy as Lazy
import qualified Web.Scotty as S
import qualified Web.Scotty.Cookie as SC
import qualified Data.Password.Bcrypt as P
import Crypto.Random (getRandomBytes)
import Imageboard.Pages (errorView, loginView)
import Imageboard.Actions (blaze)
import Imageboard.Database

randomToken :: IO Text
randomToken = TE.decodeUtf8 <$> Base64.encode <$> getRandomBytes 36 

modPage :: S.ActionM ()
modPage = do
    key <- SC.getCookie "session-token"
    case key of
        Nothing -> blaze $ loginView
        Just k -> do
            isValid <- liftIO $ checkSession k
            if isValid
                then S.text $ ("You are logged in, " <> Lazy.fromStrict k)
                else SC.deleteCookie "session-token" >> S.text "Your session token is invalid or expired"

tryLogin :: S.ActionM ()
tryLogin = do
    user <- S.param "username"
    pass <- P.mkPassword <$> S.param "password"
    hash <- liftIO $ getPasswordHash user
    case hash of
        Just h -> case P.checkPassword pass h of
                P.PasswordCheckSuccess -> do
                    key <- liftIO $ randomToken
                    liftIO $ insertSessionToken user key
                    SC.setSimpleCookie "session-token" key
                    S.redirect "/mod"
                _ -> S.text $ "Wrong password"
        Nothing -> S.text $ "Wrong username"
