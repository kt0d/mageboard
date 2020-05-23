{-# LANGUAGE OverloadedStrings #-}
module Imageboard.Pages (
    boardView
) where
import Data.List as List
import Text.Blaze.Html5((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Data.Time.Format (formatTime, defaultTimeLocale)
import Imageboard.Types

cssFile :: FilePath
cssFile = "/board.css"

space :: H.Html 
space = "\n"

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
        mconcat $ List.intersperse (H.hr ! A.class_ "invisible") $ map postView ps
    H.hr
