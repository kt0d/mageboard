{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Imageboard.Actions.Admin (
    modifyBoard,
    createBoard,
    prepareBoardEdit,
    createAccount
) where
import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class
import qualified Data.Password.Bcrypt as P
import qualified Web.Scotty as S
import Network.HTTP.Types.Status (created201, badRequest400)
import Imageboard.Pages (errorView, boardModifyPage)
import Imageboard.Actions.Common
import Imageboard.Database
import Imageboard.Types (Role(..), Board, BoardInfo(..), BoardConstraints(..))

loadBoardFormThen :: (BoardInfo -> BoardConstraints -> Action) -> Action
loadBoardFormThen action = do
    info <- (liftM3 BoardInfo)   <$> maybeParam "name"   
                        <*> maybeParam "title"
                        <*> maybeParam "subtitle"
    l <- checkBoxParam "locked"
    cs <- (liftM5 $ Constraints l) <$> maybeParam "minlen"
                        <*> maybeParam "maxlen"
                        <*> maybeParam "maxnewlines"
                        <*> maybeParam "maxreplies"
                        <*> maybeParam "maxthreads"
    case (info, cs) of
        (Just i, Just c) -> action i c
        _ ->  do
            blaze $ errorView "Insufficient params"

modifyBoard :: Action
modifyBoard = do
    maybeParam "board"
    >>= maybe 
        (blaze $ errorView "No board to modify")
        (\b -> loadBoardFormThen $ \bi bc -> do
            liftIO $ updateBoard b bi bc
            S.redirect "/mod"
        )
        
createBoard :: Action
createBoard = do
    loadBoardFormThen $ \bi bc -> do
        liftIO $ insertBoard bi bc
        S.status created201
        S.redirect "/mod"

-- | Send form for modyfing existing board. 
prepareBoardEdit :: Board -> Action
prepareBoardEdit b = do
    info <- liftIO $ getBoardInfo b
    cs <- liftIO $ getConstraints b
    case (info, cs) of
        (Just i, Just c) -> blaze $ boardModifyPage i c
        _ -> do
            S.status badRequest400
            blaze $ errorView "Board does not exist"

createAccount :: Action
createAccount = do
    pass <- P.mkPassword <$> S.param "password"
    user <- S.param "username"
    runExceptT $ do
        exists <- liftIO $ checkAccount user
        when exists $ throwError "Account with this username already exists"
        hash <- liftIO $ P.hashPassword pass
        liftIO $ insertAccount user hash Moderator
    >>= either
        (\msg -> do
            S.status badRequest400
            blaze $ errorView msg)
        (\_ -> do
            S.status created201
            blaze $ errorView "Account succesfully created")