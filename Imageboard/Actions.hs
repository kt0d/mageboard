{-# LANGUAGE OverloadedStrings #-}
module Imageboard.Actions (
    createPost
) where
import Data.Maybe
import Control.Monad
import Control.Monad.Trans
import qualified Network.HTTP.Types.Status as S
import qualified Web.Scotty as S

maybeParam p = (Just <$> S.param p) `S.rescue` (const $ return Nothing)

createPost :: S.ActionM ()
createPost = do 
    let maxPostLen = 500
            -- postAuthor  <- maybeParam "name"
            -- postEmail   <- maybeParam "email"
            -- postSubject <- maybeParam "subject"
            -- postText    <- maybeParam "comment" 
            -- case T.compareLength postText maxPostLen of
            --     GT -> S.status S.badRequest400 >> S.finish
            --     _ -> do 
            --postNumber <- liftIO $ insertPost postAuthor postEmail postSubject postText
            --files <- S.files
            --liftIO $ forM_ (take 1 files) (insertFile postNumber)
    S.redirect "/"
