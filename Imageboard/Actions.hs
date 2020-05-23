{-# LANGUAGE OverloadedStrings #-}
module Imageboard.Actions (
    createPost,
    blaze
) where
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad
import Control.Monad.Trans
import qualified Network.HTTP.Types.Status as S
import qualified Web.Scotty as S
import Text.Blaze.Html5 (Html)
import Text.Blaze.Html.Renderer.Text (renderHtml)

import Imageboard.Database
import Imageboard.Types
import Imageboard.Pages (boardView, errorView)


blaze :: Html -> S.ActionM ()
blaze = S.html . renderHtml  
  
maybeParam p = (Just <$> S.param p) `S.rescue` (const $ return Nothing)

tryMakePost :: Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Either Text PostStub
tryMakePost a e s t
    | postText    `cmp` 5    == LT = Left "Post text too short"
    | postText    `cmp` 500  == GT = Left "Post text too long"
    | postEmail   `cmp` 320  == GT = Left "Email too long"
    | postSubject `cmp` 128  == GT = Left "Subject too long"
    | postAuthor  `cmp` 64   == GT = Left "Author name too long"
    | T.count "\n" postText > 20 = Left "Too many newlines in post text"
    | otherwise = Right $ Stub postAuthor postEmail postSubject postText
    where
        cmp = T.compareLength
        postAuthor  = case a of
            Just name | not $ T.null name -> name
            _ -> "Nameless"
        postEmail   = fromMaybe "" e
        postSubject = fromMaybe "" s
        postText    = fromMaybe "" t
            

createPost :: S.ActionM ()
createPost = do 
    postAuthor  <- maybeParam "name"
    postEmail   <- maybeParam "email"
    postSubject <- maybeParam "subject"
    postText    <- maybeParam "comment" 
    case tryMakePost postAuthor postEmail postSubject postText of
        Left msg -> blaze $ errorView msg
        Right stub -> do
            postNumber <- liftIO $ insertPost stub
            --files <- S.files
            --liftIO $ forM_ (take 1 files) (insertFile postNumber)
            S.redirect "/"
