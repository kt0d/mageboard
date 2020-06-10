{-# LANGUAGE OverloadedStrings #-}
module Imageboard.Pages.Common (
    flagsToText,
    space,
    commonHtml,
    postForm,
    fileThumbLink
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
    H.body c

postForm :: Maybe Int -> H.Html
postForm parent = H.fieldset $ H.form ! A.id "postform" ! 
    A.action postUrl ! A.method "post" ! A.enctype "multipart/form-data" $ do
    H.table $ H.tbody $ do
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
            H.td ! A.colspan "2" $ H.textarea ! A.id "comment" ! A.name "comment" ! A.form "postform" ! A.rows "5" ! A.maxlength "32768" $ mempty
        H.tr $ do
            H.th $ H.label ! A.for "file" $ "File"
            H.td ! A.colspan "2" $ H.input ! A.id "file" ! A.type_ "file" ! A.name "file"
        H.tr $ do
            H.th $ H.label ! A.for "captcha" $ "Captcha"
            H.td $ H.input ! A.id "captcha" ! A.name "captcha" ! A.type_ "text" ! A.maxlength "320" ! A.autocomplete "off"
    foldMap (\n -> H.input ! A.type_ "hidden" ! A.name "parent" ! A.value (H.toValue n)) parent
    where
        postUrl = H.toValue $ case parent of
            Just n -> "/post/" ++ show n
            Nothing -> "/post"

