{-# LANGUAGE OverloadedStrings #-}
module Imageboard.Actions (
    createPost,
    blaze
) where
import Control.Monad.Except
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text.Lazy as Lazy (Text, pack, empty)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as B
import qualified Network.Wai.Parse as N (FileInfo(..))
import qualified Web.Scotty as S
import Network.HTTP.Types.Status (created201, badRequest400)
import Text.Blaze.Html5 (Html)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Debug.Trace
import Imageboard.Database
import Imageboard.Types
import Imageboard.Pages (errorView)
import Imageboard.FileUpload

-- | Send HTML created with blaze-html combinators. 
blaze :: Html -> S.ActionM ()
blaze = S.html . renderHtml  
  
maybeParam :: S.Parsable a => Lazy.Text -> S.ActionM (Maybe a)
maybeParam p = (Just <$> S.param p) `S.rescue` (const $ return Nothing)

maybeFile :: S.ActionM (Maybe FileData)
maybeFile = listToMaybe <$> filter (not . B.null . N.fileContent) <$> map snd <$> S.files 

tryMkStub :: Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Bool ->  Either Text PostStub
tryMkStub a e s t hasFile
    | postText    #< 5 && not hasFile   = Left "Post text too short"
    | postText    #> 500                = Left "Post text too long"
    | postEmail   #> 320                = Left "Email too long"
    | postSubject #> 128                = Left "Subject too long"
    | postAuthor  #> 64                 = Left "Author name too long"
    | T.count "\n" postText > 20        = Left "Too many newlines in post text"
    | otherwise = Right $ Stub postAuthor postEmail postSubject postText
    where
        x #< y = T.compareLength x y == LT
        x #> y = T.compareLength x y == GT
        postAuthor  = case a of
            Just name | not $ T.null name -> name
            _ -> "Nameless"
        postEmail   = fromMaybe "" e
        postSubject = fromMaybe "" s
        postText    = fromMaybe "" t

tryInsertFile :: FileData -> ExceptT Text IO Int
tryInsertFile fdata = do
    (f, fPath) <- liftEither $ tryMkFile fdata
    exists <- liftIO $ getFileId $ filename f
    case exists of
        Just fileId -> return fileId
        Nothing -> do
            newF <- saveFile f fdata fPath
            liftIO $ insertFile newF

tryInsertThread :: PostStub -> Maybe FileData -> ExceptT Text IO Int
tryInsertThread stub mdata = case mdata of
    Just fdata -> do
        fileId <- tryInsertFile fdata
        liftIO $ insertThreadWithFile stub fileId
    Nothing -> 
        liftIO $ insertThread stub 

tryInsertPost :: Int -> PostStub -> Maybe FileData -> ExceptT Text IO Int
tryInsertPost p stub mdata = case mdata of
    Just fdata -> do
        fileId <- tryInsertFile fdata
        liftIO $ insertPostWithFile p stub fileId
    Nothing -> 
        liftIO $ insertPost p stub 


-- | This action will try to insert post sent by the user.
-- In case of failure it will send user appropriate error page.
createPost :: Maybe Int -> S.ActionM ()
createPost p = do 
    postAuthor  <- maybeParam "name"
    postEmail   <- maybeParam "email"
    postSubject <- maybeParam "subject"
    postText    <- maybeParam "comment" 
    postFile    <- maybeFile
    result <- liftIO $ runExceptT $ do 
        stub <- liftEither $ tryMkStub postAuthor postEmail postSubject postText (isJust postFile)
        case p of
            Just n -> do
                exists <- liftIO $ checkThread n
                if exists 
                    then tryInsertPost n stub postFile
                    else throwError "Thread does not exist"
            Nothing -> tryInsertThread stub postFile
    case result of
        Left msg -> do
            S.status badRequest400
            blaze $ errorView msg
        Right _ -> do
            S.status created201
            S.redirect $ "/" <> fromMaybe Lazy.empty (Lazy.pack <$> show <$> p)
