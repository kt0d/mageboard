{-# LANGUAGE OverloadedStrings #-}
module Imageboard.Pages (
    boardView,
    errorView
) where
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.List as List
import Text.Blaze.Html5((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Printf (printf)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Imageboard.Types
import Imageboard.Markup

cssFile :: FilePath
cssFile = "/board.css"

space :: H.Html 
space = "\n"

commonHtml :: H.Html -> H.Html
commonHtml c = H.docTypeHtml $ do
    H.head $
        H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href (H.preEscapedStringValue cssFile)
    H.body c

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

thumbnailDim :: Dimensions -> Dimensions
thumbnailDim (Dim w h)
    | w <= 200 && h <= 200  = Dim w h
    | w > h                 = Dim 200 $ (200*h) `quot` w
    | otherwise             = flip Dim 200 $ (200*w) `quot` h

sizeFormat :: Int -> Text
sizeFormat = T.pack . toFormat
    where 
        toFormat n
            | n > 1048576 = printf "%.3f MiB" (x/1048576)
            | n > 1024 = printf "%.3f KiB" (x/1024)
            | otherwise = printf "%d B" n
            where x = fromIntegral n :: Double

fileBox :: File -> H.Html
fileBox f = do
    let link = H.toValue $ mconcat ["/media/", filename f]
    let thumbLink = H.toValue $ mconcat ["/media/thumb/", if isImage $ ext f then filename f else filename f `T.append` ".jpg"]
    let (Dim w h) = fromMaybe (Dim 0 0) $ thumbnailDim <$> dim f
    H.div ! A.class_ "post-file-info" $ do
        H.a ! A.type_ "blank" ! A.href link $ H.text "File"
        space
        H.toHtml $ show $ ext f
        space
        H.toHtml ("(" :: Text)
        H.a ! H.customAttribute "download" "" ! A.href link $ H.text "dl"
        H.toHtml $ ") " `T.append` (sizeFormat $ size f)
    if isAudio $ ext f 
    then 
        H.audio ! A.preload "none" ! A.loop "" ! A.controls "" $
            H.source ! A.type_ (if ext f == OGG then "audio/ogg" else "audio/mpeg") ! A.src link
    else 
        H.a ! A.type_ "blank" ! A.href link  $ 
            H.img ! A.class_ "post-file-thumbnail" ! 
            A.width (H.toValue w) ! A.height (H.toValue h) ! A.src thumbLink

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
        foldMap fileBox (file p)
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