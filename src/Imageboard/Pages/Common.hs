{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Imageboard.Pages.Common (
    renderFlags,
    space,
    commonHtml,
    addTopBottom,
    threadForm,
    replyForm,
    fileThumbLink,
    formatDate
) where
import Data.Text (Text)
import qualified Data.Text as T
import Text.Blaze.Html5((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Data.Time (formatTime, UTCTime, defaultTimeLocale, utcToLocalTime)
import Imageboard.Types (ThreadFlags(..), File(..), isImage, Board)
import qualified Imageboard.Config as Config

-- | Format date to YYYY-MM-DD HH:MM:SS.
formatDate :: UTCTime -> String
formatDate = formatTime defaultTimeLocale "%F %T" . utcToLocalTime Config.timezone

-- | Render flags that apply to a thread.
renderFlags :: ThreadFlags -> H.Html
renderFlags Flags{..} = mconcat
    [ if sticky   then H.span ! A.title "Sticky"    ! A.class_ "sticky-icon"   $ "(S)" else mempty
    , if lock     then H.span ! A.title "Locked"    ! A.class_ "lock-icon"     $ "(L)" else mempty
    , if autosage then H.span ! A.title "Autosage"  ! A.class_ "autosage-icon" $ "(A)" else mempty
    , if cycle_   then H.span ! A.title "Cycle"     ! A.class_ "cycle-icon"    $ "(C)" else mempty]

-- | A space character.
space :: H.Html 
space = "\n"

fileThumbLink :: File -> Text
fileThumbLink f = mconcat ["/media/thumb/"
    , if isImage $ ext f then filename f else filename f `T.append` ".jpg"]

-- | Render HTML with title and navbar that includes list of boards.
commonHtml :: Text -- ^ Title
        -> [Board] -- ^ List of boards
        -> H.Html -- ^ Inner HTML
        -> H.Html
commonHtml pageTitle bs c = H.docTypeHtml $ do
    H.head $ do
        H.meta ! A.charset "UTF-8"
        H.title $ H.text pageTitle
        let stylesheet = H.link ! A.type_ "text/css"
        stylesheet ! A.rel "stylesheet" ! A.href Config.cssFile
        foldMap 
            (\(l,t) -> stylesheet ! A.rel  "stylesheet" ! A.href l ! A.title t) 
            (take 1 Config.cssStyles)
        foldMap 
            (\(l,t) -> stylesheet ! A.rel "alternate stylesheet" ! A.href l ! A.title t) 
            (drop 1 Config.cssStyles)
    H.body $ do
        H.a ! A.id "top-of-page" $ mempty
        navBar bs
        c
        H.a ! A.id "bottom-of-page" $ mempty

navBar :: [Board] -> H.Html
navBar bs = H.div ! A.id "topbar" $ 
    H.nav ! A.id "topnav" $ do
        H.ul ! A.id "navigation" $ do
            H.li $ H.a ! A.href "/" $ "home"
            H.li $ H.a ! A.href "/recent" $ "recent"
            H.li $ H.a ! A.href "/mod" $ "mod"
        H.ul ! A.id "board-navigation" $
            flip foldMap bs $ \b -> H.li $ H.a ! A.href ("/" <> H.toValue b) $ H.text b

        H.ul ! A.id "shortcuts" $ do
            H.li $ H.details ! A.id "infobox" $ do
                H.summary "info"
                H.div ! A.class_ "infobox-content" $ do
                    "mageboard 2020"
                    H.hr
                    "Allowed file formats: JPG | PNG | GIF | WEBM | MP4 | MP3 | OGG | PDF | EPUB | SWF"
                    H.hr
                    "Formattting options include:"
                    H.ul $ do
                        H.li "'''bold text'''"
                        H.li "''italic text''"
                        H.li "**spoiler**"
                        H.li "==red text=="
                        H.li ">greentext"
                        H.li "<pinktext"
                        H.li "```code``` (multiline allowed)"

-- | Add navigation links that bring to top and bottom of the page.
addTopBottom :: H.Html -> H.Html
addTopBottom c = do
    H.nav $ do
        H.a ! A.href "#bottom-of-page" $ "[Go to bottom]"
    H.hr
    c
    H.hr
    H.nav $ do
        H.a ! A.href "#top-of-page" $ "[Go to top]"
        space
        H.a ! A.href "#postform" $ "[Open form]"

-- | A form for creating threads on given board.
threadForm :: Board -> H.Html
threadForm b = H.div ! A.class_ "form-container" ! A.id "postform" $ H.fieldset $ H.form !
    A.action postUrl ! A.method "post" ! A.enctype "multipart/form-data" $ do
        postFormTable
    where
        postUrl = "/post/" <> H.toValue b 
    
-- | A form for creating replies to thread specified by board and number.
replyForm :: Board -> Int -> H.Html
replyForm b n = H.div ! A.class_ "form-container" ! A.id "postform" $ H.fieldset $ H.form !
    A.action postUrl ! A.method "post" ! A.enctype "multipart/form-data" $ do
        postFormTable
    where 
        postUrl = "/post/" <> H.toValue b <> "/" <> H.toValue n

postFormTable :: H.Html
postFormTable = H.table $ H.tbody $ do
    H.tr $ do
        H.th $ H.label ! A.for "name" $ "Name"
        H.td $ H.input ! A.id "name" ! A.name "name" ! A.type_ "text" ! A.maxlength "64" 
        H.td $ H.a ! A.class_ "close-button" ! A.href "#!" $ "[X]"
    H.tr $ do
        H.th $ H.label ! A.for "email" $ "Email"
        H.td $ H.input ! A.id "email" ! A.name "email" ! A.type_ "text" ! A.maxlength "320"
    H.tr $ do
        H.th $ H.label ! A.for "subject" $ "Subject"
        H.td $ H.input ! A.id "subject" ! A.name "subject" ! A.type_ "text" ! A.maxlength "128" 
        H.td $ H.input ! A.type_ "submit" ! A.value "Post" 
    H.tr $ do
        H.th $ H.label ! A.for "comment" $ "Comment"
        H.td ! A.colspan "2" $ H.textarea ! A.id "comment" ! A.name "comment" ! A.rows "5" ! A.maxlength "32768" $ mempty
    H.tr $ do
        H.th $ H.label ! A.for "file" $ "File"
        H.td ! A.colspan "2" $ H.input ! A.id "file" ! A.type_ "file" ! A.name "file"
    H.tr $ do
        H.th $ H.label ! A.for "captcha" $ "Captcha"
        H.td $ H.input ! A.id "captcha" ! A.name "captcha" ! A.type_ "text" ! A.maxlength "10" ! A.autocomplete "off"
        H.td $ H.img ! A.alt "captcha image" ! A.class_ "captcha" ! A.src "/captcha.png"