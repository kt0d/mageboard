{-# LANGUAGE OverloadedStrings #-}
module Main where
import Data.List
import Data.Maybe
import Control.Monad
import Control.Monad.Trans
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Network.Wai.Middleware.RequestLogger as WAI (logStdoutDev)
import qualified Network.Wai.Middleware.Static as WAI 
import qualified Web.Scotty as S
import qualified Network.HTTP.Types.Status as S
import qualified Database.SQLite.Simple as DB
import qualified Database.SQLite.Simple.FromRow as DB (fieldWith)
import qualified Database.SQLite.Simple.FromField as DB (FieldParser, fieldData, returnError)
import qualified Database.SQLite.Simple.Ok as DB (Ok(..))
import qualified Database.SQLite.Simple.Time.Implementation as DB
import qualified Database.SQLite3 as DirectDB (open, close, exec)
import Text.Blaze.Html5((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.Format (formatTime, defaultTimeLocale)

postsDb :: FilePath
postsDb = "posts.db" 

schemaFile :: FilePath
schemaFile = "schema.sql"

cssFile :: FilePath
cssFile = "/board.css"

data Post = Post {
    number :: Int,
    date :: UTCTime,
    author :: Text,
    email :: Text,
    subject :: Text,
    text :: Text
} deriving (Show)

data File = File {
    filename :: Text,
    size :: Int,
    width :: Int,
    height :: Int
} deriving (Show)


instance DB.FromRow Post where
    fromRow = Post  <$> DB.field 
                    <*> DB.fieldWith parseDate 
                    <*> DB.field 
                    <*> DB.field 
                    <*> DB.field
                    <*> DB.field
        where 
            parseDate :: DB.FieldParser UTCTime
            parseDate f = case DB.fieldData f of 
                -- INTEGER as Unix Time, the number of seconds since 1970-01-01 00:00:00 UTC. 
                DB.SQLInteger x -> DB.Ok . posixSecondsToUTCTime $ fromIntegral x
                -- TEXT as ISO8601 strings ("YYYY-MM-DD HH:MM:SS.SSS"). 
                DB.SQLText x -> case DB.parseUTCTime x of 
                    Left s -> DB.returnError DB.ConversionFailed f s
                    Right t -> DB.Ok $ t
                _ -> DB.returnError DB.Incompatible f "expecting an SQLInteger column type"

setupDb :: IO ()
setupDb = do
    conn <- DirectDB.open $ T.pack postsDb
    schema <- T.readFile schemaFile
    DirectDB.exec conn schema
    DirectDB.close conn

insertPost :: Text -> Text -> Text -> Text -> IO Int
insertPost n e s t = DB.withConnection postsDb $ \c -> do
    DB.executeNamed c   "INSERT INTO Posts (Name, Email, Subject, Text) \
                        \VALUES (:name, :email, :subject, :text)" 
                        [ ":name"     DB.:= n
                        , ":email"    DB.:= e
                        , ":subject"  DB.:= s
                        , ":text"     DB.:= t]
    fromIntegral <$> DB.lastInsertRowId c
    

getPosts :: IO [Post]
getPosts = DB.withConnection postsDb $ \c ->
    DB.query_ c "SELECT * FROM Posts ORDER BY Number DESC" 


postForm :: H.Html
postForm = H.fieldset $ H.form ! A.id "postform" ! 
    A.action "/post" ! A.method "post" ! A.enctype "multipart/form-data" $ do
    H.label ! A.for "name" $ "Name"
    H.input ! A.id "name" ! A.name "name" ! A.type_ "text" ! A.maxlength "64" 
    H.br
    H.label ! A.for "email" $ "Email"
    H.input ! A.id "email" ! A.name "email" ! A.type_ "text" ! A.maxlength "320"
    H.br
    H.label ! A.for "subject" $ "Subject"
    H.input ! A.id "subject" ! A.name "subject" ! A.type_ "text" ! A.maxlength "128" 
    H.br
    H.label ! A.for "comment" $ "Comment"
    H.textarea ! A.id "comment" ! A.name "comment" ! A.form "postform" 
        ! A.rows "5" ! A.maxlength "32768" $ mempty
    H.br
    H.label ! A.for "file" $ "File"
    H.input ! A.id "file" ! A.type_ "file" ! A.name "file"
    H.br
    H.input ! A.type_ "submit" ! A.value "Post"
    H.br

space :: H.Html 
space = "\n"

postView :: Post -> H.Html 
postView p = H.div ! A.class_ "post-container" ! A.id postid $ do
    H.div ! A.class_ "post" $ do
        H.div ! A.class_ "post-header" $ do
            H.span ! A.class_ "post-subject" $ H.text $ subject p
            space
            H.span ! A.class_ "post-name" $ do 
                let emailTag = H.a ! A.class_ "post-email" ! A.href (H.textValue $ mconcat ["mailto:", email p])
                (case email p of "" -> id; _ -> emailTag) $ H.text $ author p
            space
            H.span ! A.class_ "post-date" $ 
                H.time ! A.datetime (H.toValue datetime) $ (H.string datetime)
            space
            H.span ! A.class_ "post-number" $ 
                H.a ! A.href (mconcat ["#", postid]) $ 
                    H.string $ mconcat ["No.", show $ number p]
        H.div ! A.class_ "post-comment" $ H.text $ text p
    H.br
    where  
        postid = H.preEscapedStringValue $ mconcat ["postid", show $ number p]
        datetime = formatTime defaultTimeLocale "%F %T" (date p)


boardView :: [Post] -> H.Html
boardView ps = do 
    H.head $
        H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href (H.preEscapedStringValue cssFile)
    postForm
    H.hr 
    H.div ! A.class_ "content" $ do
        mconcat $ intersperse (H.hr ! A.class_ "invisible") $ map postView ps
    H.hr

blaze :: H.Html -> S.ActionM ()
blaze = S.html . renderHtml

main :: IO ()
main = do
    setupDb
    S.scotty 3000 $ do
        S.middleware WAI.logStdoutDev
        S.middleware $ WAI.staticPolicy $ WAI.noDots WAI.>-> 
            WAI.addBase "static" WAI.<|> WAI.addBase "media"
        S.get "/" $ do
            posts <- liftIO getPosts
            blaze $ boardView posts
        S.post "/post" $ do
            let maxPostLen = 500
            let paramOr p a = S.param p `S.rescue` (const $ return a)
            postAuthor  <- "name"    `paramOr` "Nameless"
            postEmail   <- "email"   `paramOr` ""
            postSubject <- "subject" `paramOr` ""
            postText    <- "comment" `paramOr` ""
            -- case T.compareLength postText maxPostLen of
            --     GT -> S.status S.badRequest400 >> S.finish
            --     _ -> do 
            postNumber <- liftIO $ insertPost postAuthor postEmail postSubject postText
            --files <- S.files
            --liftIO $ forM_ (take 1 files) (insertFile postNumber)
            S.redirect "/"
