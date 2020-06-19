{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Imageboard.Actions.Admin (
    modifyBoard,
    createBoard
) where
import Control.Monad.Except
import qualified Web.Scotty as S
import Network.HTTP.Types.Status (unauthorized401)
import Imageboard.Pages (errorView)
import Imageboard.Utils
import Imageboard.Database
import Imageboard.Types (Role(..), BoardInfo(..), BoardConstraints(..))

loadBoardFormThen :: (BoardInfo -> BoardConstraints -> S.ActionM ()) -> S.ActionM ()
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

modifyBoard :: S.ActionM ()
modifyBoard = do
    board <- maybeParam "board"
    maybe 
        (blaze $ errorView "No board to modify")
        (\b -> loadBoardFormThen $ \bi bc -> do
            liftIO $ updateBoard b bi bc
            S.redirect "/mod"
        )
        board
        
createBoard :: S.ActionM ()
createBoard = do
    loadBoardFormThen $ \bi bc -> do
        liftIO $ insertBoard bi bc
        S.redirect "/mod"