{-# LANGUAGE OverloadedStrings #-}
module Imageboard.Pages.Common (
    flagsToText,
    space,
    commonHtml,
    addTopBottom,
    threadForm,
    replyForm,
    fileThumbLink,
    navBar
) where
import Data.Text (Text)
import qualified Data.Text as T
import Text.Blaze.Html5((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Imageboard.Types (ThreadInfo(..), File(..), isImage)

flagsToText :: ThreadInfo -> Text
flagsToText ti = 
    if sticky ti    then "(S)" else T.empty <>
    if lock ti      then "(L)" else T.empty <>
    if autosage ti  then "(A)" else T.empty <>
    if cycle_ ti    then "(C)" else T.empty

cssFile :: FilePath
cssFile = "/board.css"

space :: H.Html 
space = "\n"

fileThumbLink :: File -> Text
fileThumbLink f = mconcat ["/media/thumb/"
    , if isImage $ ext f then filename f else filename f `T.append` ".jpg"]

commonHtml :: H.Html -> H.Html
commonHtml c = H.docTypeHtml $ do
    H.head $
        H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href (H.preEscapedStringValue cssFile)
    H.body $ do
        H.a ! A.id "top-of-page" $ mempty
        navBar
        c
        H.a ! A.id "bottom-of-page" $ mempty

navBar :: H.Html
navBar = H.div ! A.id "topbar" $ 
    H.nav ! A.id "topnav" $ do
        H.ul ! A.id "navigation" $ do
            H.li $ H.a ! A.href "/" $ "home"
            H.li $ H.a ! A.href "/recent" $ "recent"
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

threadForm :: H.Html
threadForm = H.div ! A.class_ "form-container" ! A.id "postform" $ H.fieldset $ H.form !
    A.action "/post" ! A.method "post" ! A.enctype "multipart/form-data" $ do
        postFormTable

replyForm :: Int -> H.Html
replyForm n = H.div ! A.class_ "form-container" ! A.id "postform" $ H.fieldset $ H.form !
    A.action postUrl ! A.method "post" ! A.enctype "multipart/form-data" $ do
        postFormTable
        H.input ! A.type_ "hidden" ! A.name "parent" ! A.value (H.toValue n)
    where 
        postUrl = "/post/" <> H.toValue n

postFormTable :: H.Html
postFormTable = H.table $ H.tbody $ do
    H.tr $ do
        H.th $ H.label ! A.for "name" $ "Name"
        H.td $ H.input ! A.id "name" ! A.name "name" ! A.type_ "text" ! A.maxlength "64" 
        H.td $ H.a ! A.class_ "close-button" ! A.href "##" $ "[X]"
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
        H.td $ H.input ! A.id "captcha" ! A.name "captcha" ! A.type_ "text" ! A.maxlength "320" ! A.autocomplete "off"