{-# LANGUAGE OverloadedStrings #-}
module Main where
import Data.List
import Control.Monad
import Control.Monad.Trans
import Data.Text (Text)
import qualified Data.Text as T
import qualified Network.Wai.Middleware.RequestLogger as WAI (logStdoutDev)
import qualified Network.Wai.Middleware.Static as WAI 
import qualified Web.Scotty as S
import qualified Network.HTTP.Types.Status as S
import qualified Database.SQLite.Simple as DB
import qualified Database.SQLite.Simple.FromRow as DB (fieldWith)
import qualified Database.SQLite.Simple.FromField as DB (FieldParser, fieldData, returnError)
import qualified Database.SQLite.Simple.Ok as DB (Ok(..))
import qualified Database.SQLite.Simple.Time.Implementation as DB
import Text.Blaze.Html5((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.Format (formatTime, defaultTimeLocale)

data Post = Post {
    number :: Int,
    comment :: Text,
    date :: UTCTime
}

instance DB.FromRow Post where
    fromRow = Post <$> DB.field <*> DB.field <*> DB.fieldWith parseDate
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

postsDb :: String
postsDb = "posts.db" 

setupDb :: IO ()
setupDb = DB.withConnection postsDb $ \c -> do 
    DB.execute_ c   "CREATE TABLE IF NOT EXISTS Posts \
                    \(Number INTEGER PRIMARY KEY, Text TEXT, Date INTEGER)"
    DB.execute_ c   "CREATE TRIGGER IF NOT EXISTS set_post_date AFTER INSERT ON Posts\n\
                    \BEGIN\n\
                    \UPDATE Posts SET Date = strftime('%s','now') WHERE  ROWID = NEW.ROWID; \n\
                    \END;"

insertPost :: Text -> IO ()
insertPost t = DB.withConnection postsDb $ \c -> 
    DB.executeNamed c "INSERT INTO Posts (Text) VALUES (:text)" [":text" DB.:= t]

getPosts :: IO [Post]
getPosts = DB.withConnection postsDb $ \c ->
    DB.query_ c "SELECT * FROM Posts ORDER BY Number DESC" 

space :: H.Html 
space = H.preEscapedToMarkup ("&nbsp" :: Text)

postForm :: H.Html
postForm = H.fieldset $ H.form ! A.id "postform" ! 
    A.action "/post" ! A.method "post" ! A.enctype "multipart/form-data" $ do
    H.label ! A.for "comment" $ "Comment"
    H.textarea ! A.id "comment" ! A.name "comment" ! A.form "postform" ! A.rows "5" $ mempty
    H.br
    H.input ! A.type_ "submit" ! A.value "Post"
    H.br

postView :: Post -> H.Html 
postView p = H.div ! A.class_ "post-container" ! A.id postid $ do
    H.div ! A.class_ "post" $ do
        H.div ! A.class_ "post-header" $ do
            H.span ! A.class_ "post-name" $ "Anonymous"
            space
            H.span ! A.class_ "post-date" $ 
                H.time ! A.datetime (H.toValue datetime) $ (H.string datetime)
            space
            H.span ! A.class_ "post-number" $ 
                H.a ! A.href (mconcat ["#", postid]) $ 
                    H.preEscapedString $ mconcat ["No.", show $ number p]
        H.div ! A.class_ "post-comment" $ do
            H.text $ comment p
    H.br
    where  
        postid = H.preEscapedStringValue $ mconcat ["postid", show $ number p]
        datetime = formatTime defaultTimeLocale "%F %T" (date p)


boardView :: [Post] -> H.Html
boardView ps = do 
    H.style $ H.text css
    postForm
    H.hr 
    H.div ! A.class_ "content" $ do
        mconcat $ intersperse (H.hr ! A.class_ "invisible") (map postView ps)
    H.hr

blaze :: H.Html -> S.ActionM ()
blaze = S.html . renderHtml

main :: IO ()
main = do
    setupDb
    S.scotty 3000 $ do
        S.middleware WAI.logStdoutDev
        S.middleware $ WAI.staticPolicy (WAI.noDots WAI.>-> WAI.addBase "static")
        S.get "/" $ do
            posts <- liftIO getPosts
            blaze $ boardView posts
        S.post "/post" $ do
            let maxPostLen = 500
            t <- S.param "comment"
            case T.compareLength t maxPostLen of
                GT -> S.status S.badRequest400
                _ -> do
                    liftIO $ insertPost t
                    S.redirect "/"

css :: Text
css  = "body { background-color: #DFE; } \
\ hr { position: relative } \
\ fieldset { border: none } \
\ form#postform { top: 30px; right: 10px; background-color: #BDC; border: \
\   1px solid #000; padding: 5px; } \
\ form label { background-color: #ACB; text-align: left; display: inline-block; \
\   width: 100px; vertical-align: top; margin-right: 5px; padding: 4px; } \
\ form#postform>label { width: 7ch;} \
\ textarea { margin-right: 5px; margin-bottom: 5px; border: 1px solid #000; \
\   width: calc(100% - 75px); resize: both; } \
\ div.post-container { padding-top: 25px; margin-top: -25px; } \
\ div.post { background-color: #BDC; padding: 10px; border: 1px solid #000; \
\ display: inline-block; margin-bottom: 10px; max-width: 1150px; word-wrap: break-word; } \
\ div.post>div.post-header { margin-bottom: 5px; } \
\ div.post>div.post-header>span.post-name { font-weight: bold; color: #070; } \
\ div.post-comment { white-space: pre-wrap;} \
\ .invisible { display: none; } "
