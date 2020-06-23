{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Imageboard.Actions.Auth (
    modPage,
    tryLogin,
    logout,
    allowLoggedIn,
    allowAdmin,
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
import Imageboard.Pages (errorView, loginView, loggedInPage, adminLoggedInPage)
import Imageboard.Actions.Common
import Imageboard.Database
import Imageboard.Types (AccountInfo(..),SessionKey, Role(..))

setSessionCookie :: SessionKey -> Action
setSessionCookie st = SC.setSimpleCookie "session-token" $ st <> "; SameSite=Strict; Max-Age=3600"

randomToken :: IO SessionKey
randomToken = TE.decodeUtf8 <$> Base64.encode <$> getRandomBytes 72

disallow :: Text -> Action
disallow msg = do
    S.status unauthorized401
    SC.deleteCookie "session-token"
    blaze $ errorView msg

allowByRole :: (Role -> Bool) -> Action -> Action
allowByRole f action = do
    key <- SC.getCookie "session-token"
    runExceptT $ do
        k <- maybetoExcept "Unathorized" key
        AccountInfo{..} <- maybetoExcept "Your session token is invalid or expired"
            =<< (liftIO $ getAccountByToken k)
        if f role 
            then return ()
            else throwError "Insufficient permissions"
    >>= either
        disallow
        (const action)

-- | Allow any logged in user to perform an action.
allowLoggedIn :: Action -> Action
allowLoggedIn = allowByRole (const True)

-- | Allow admin user to perform an action.
allowAdmin :: Action -> Action
allowAdmin = allowByRole (==Admin)

-- | If user is logged in, render his account page. 
-- Otherwise render login form.
modPage :: Action
modPage = do
    key <- SC.getCookie "session-token"
    case key of
        Nothing -> blaze $ loginView
        Just k -> do
            runExceptT $ do
                acc@(AccountInfo{..}) <- maybetoExcept "Your session token is invalid or expired" 
                    =<< (liftIO $ getAccountByToken k)
                case role of
                    Admin -> do 
                        bs <- liftIO $ getBoardInfos
                        as <- liftIO $ getAccounts
                        lift $ blaze $ adminLoggedInPage acc bs as
                    _ -> do
                        bs <- liftIO $ getBoardNames 
                        lift $ blaze $ loggedInPage acc bs
            >>= either disallow (const S.finish)

-- | Try to log in a user with supplied data from login form.
tryLogin :: Action
tryLogin = do
    user <- S.param "username"
    pass <- P.mkPassword <$> S.param "password"
    hash <- liftIO $ getPasswordHash user
    case hash of
        Just h -> case P.checkPassword pass h of
                P.PasswordCheckSuccess -> do
                    key <- liftIO $ randomToken
                    liftIO $ insertSessionToken user key
                    setSessionCookie key
                    S.redirect "/mod"
                _ -> disallow "Wrong password"
        Nothing -> disallow "Wrong username"

-- | Log out a user.
logout :: Action
logout = do
    key <- SC.getCookie "session-token"
    SC.deleteCookie "session-token"
    case key of 
        Nothing -> S.redirect "/"
        Just k -> do
            liftIO $ removeSessionToken k
            S.redirect "/"
        
-- | Try to change password of a user.
changePass :: Action
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
        (const $ blaze $ errorView "Password succesfully changed.")
