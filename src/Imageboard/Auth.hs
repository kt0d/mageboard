{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Imageboard.Auth (
    modPage,
    tryLogin,
    logout,
    allowLoggedIn,
    changePass
) where
import Control.Monad.Except
import qualified Data.ByteString.Base64 as Base64
import Data.Text (Text)
import qualified Data.Text.Encoding as TE 
import qualified Web.Scotty as S
import qualified Web.Scotty.Cookie as SC
import Network.HTTP.Types.Status (unauthorized401)
import qualified Data.Password.Bcrypt as P
import Crypto.Random (getRandomBytes)
import Imageboard.Pages (errorView, loginView, loggedInPage)
import Imageboard.Utils
import Imageboard.Database
import Imageboard.Types (AccountInfo(..),SessionKey)

randomToken :: IO SessionKey
randomToken = TE.decodeUtf8 <$> Base64.encode <$> getRandomBytes 72

maybetoExcept :: Monad m => b -> Maybe a -> ExceptT b m a
maybetoExcept e = liftEither . maybe (Left e) Right 

disallow :: Text -> S.ActionM ()
disallow msg = do
    S.status unauthorized401
    SC.deleteCookie "session-token"
    blaze $ errorView msg

allowLoggedIn :: S.ActionM () -> S.ActionM ()
allowLoggedIn action = do
    key <- SC.getCookie "session-token"
    case key of
        Nothing -> disallow "Unathorized" 
        Just k -> do
            session <- liftIO $ checkSession k
            if session 
                then action
                else disallow "Unathorized" 

modPage :: S.ActionM ()
modPage = do
    key <- SC.getCookie "session-token"
    case key of
        Nothing -> blaze $ loginView
        Just k -> do
            account <- liftIO $ getAccountByToken k
            case account of
                Just a -> blaze $ loggedInPage a
                Nothing -> do
                    disallow "Your session token is invalid or expired"

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
                _ -> disallow "Wrong password"
        Nothing -> disallow "Wrong username"

logout :: S.ActionM ()
logout = do
    key <- SC.getCookie "session-token"
    case key of 
        Nothing -> S.redirect "/"
        Just k -> do
            liftIO $ removeSessionToken k
            S.redirect "/"
        
changePass :: S.ActionM ()
changePass = do
    key <- SC.getCookie "session-token"
    old <- P.mkPassword <$> S.param "old-password"
    new <- P.mkPassword <$> S.param "new-password"
    runExceptT $ do
        k <- maybetoExcept "Unathorized" key
        AccountInfo{..} <- maybetoExcept "Your session token is invalid or expired"
            =<< (liftIO $ getAccountByToken k)
        h <- maybetoExcept "Wrong username"
            =<< (liftIO $ getPasswordHash user)
        case P.checkPassword old h of
            P.PasswordCheckSuccess ->
                P.hashPassword new
                >>= liftIO . changePassword user
            _ -> throwError "Wrong password"
    >>= either
        disallow
        (const $ S.text "ok")
        

