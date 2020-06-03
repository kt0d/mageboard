{-# LANGUAGE OverloadedStrings #-}
module Imageboard.Pages (
    boardView,
    errorView
) where
import Data.Text (Text)
import Data.Text as T
import Data.List as List
import Text.Blaze.Html5((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Data.Time.Format (formatTime, defaultTimeLocale)
import Imageboard.Types
import Imageboard.Markup

cssFile :: FilePath
cssFile = "/board.css"

space :: H.Html 
space = "\n"

commonHtml :: H.Html -> H.Html
commonHtml content = H.docTypeHtml $ do
    H.head $
        H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href (H.preEscapedStringValue cssFile)
    H.body content

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

imageBox :: File -> H.Html
imageBox f = do
    let link = H.toValue $ mconcat ["/media/", filename f]
    H.a ! A.type_ "blank" ! A.href link  $ 
        H.img ! A.class_ "post-file-thumbnail" ! A.width "200" ! A.height "200" ! A.src link

postView :: Post -> H.Html 
postView p = H.div ! A.class_ "post-container" ! A.id postNumber $ do
    H.div ! A.class_ "post" $ do
        H.div ! A.class_ "post-header" $ do
            H.span ! A.class_ "post-subject" $ H.toHtml $ postSubject
            space
            H.span ! A.class_ "post-name" $ do 
                (if T.null postEmail then id else emailTag) $ H.toHtml $ postAuthor
            space
            H.span ! A.class_ "post-date" $ 
                H.time ! A.datetime (H.toValue postDate) $ (H.toHtml postDate)
            space
            H.span ! A.class_ "post-number" $ 
                H.a ! A.href (mconcat ["#", postNumber]) $ 
                    H.string $ mconcat ["No.", show $ number p]
        foldMap imageBox (file p)
        H.div ! A.class_ "post-comment" $ H.preEscapedToHtml postText
    H.br
    where  
        emailTag = H.a ! A.class_ "post-email" ! A.href (H.toValue $ mconcat ["mailto:", postEmail])
        postNumber = H.preEscapedToValue $ mconcat ["postid", show $ number p]
        postDate = formatTime defaultTimeLocale "%F %T" (date p)
        postAuthor = author $ content p
        postEmail = email $ content p
        postSubject = subject $ content p
        postText = (if postEmail == "nofo" then escapeHTML else formatPost) $ text $ content p

boardView :: [Post] -> H.Html
boardView ps = commonHtml $ do 
    postForm
    H.hr 
    H.div ! A.class_ "content" $ do
        mconcat $ List.intersperse (H.hr ! A.class_ "invisible") $ postView <$> ps
    H.hr

errorView :: Text -> H.Html
errorView msg = commonHtml $ do
    H.div ! A.class_ "content" $
        H.div ! A.class_ "container narrow" $ H.text msg