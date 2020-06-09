{-# LANGUAGE OverloadedStrings #-}
module Imageboard.Pages (
    boardView,
    errorView,
    threadView
) where
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.String (IsString)
import qualified Data.Text as T
import qualified Data.List as List
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

postForm :: Maybe Int -> H.Html
postForm parent = H.fieldset $ H.form ! A.id "postform" ! 
    A.action "/post" ! A.method "post" ! A.enctype "multipart/form-data" $ do
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

fileThumbLink :: File -> Text
fileThumbLink f = mconcat ["/media/thumb/"
    , if isImage $ ext f then filename f else filename f `T.append` ".jpg"]

fileBox :: File -> H.Html
fileBox f = do
    H.div ! A.class_ "post-file-info" $ do
        H.a ! A.type_ "blank" ! A.href link $ H.text "File"
        space
        H.toHtml $ show $ ext f
        space
        H.toHtml ("(" :: Text)
        H.a ! H.customAttribute "download" "" ! A.href link $ H.text "dl"
        H.toHtml $ ") " `T.append` (sizeFormat $ size f)
        foldMap dimFormat $ dim f
    case ext f of
        e | isAudio e -> showAudio
        EPUB -> showFallback "epub.png"
        SWF -> showFallback "swf.png"
        _ -> showThumb
    where
        dimFormat (Dim x y) = space >> (H.string $ printf "%dx%d" x y)
        link = H.toValue $ mconcat ["/media/", filename f]
        thumbLink = H.toValue $ fileThumbLink f
        showAudio = H.audio ! A.preload "none" ! A.loop "" ! A.controls "" $
            H.source ! A.type_ (if ext f == OGG then "audio/ogg" else "audio/mpeg") ! A.src link
        showImage :: H.AttributeValue -> Dimensions -> H.Html
        showImage path (Dim w h) = H.a ! A.type_ "blank" ! A.href link $ 
            H.img ! A.class_ "post-file-thumbnail" ! 
            A.width (H.toValue w) ! A.height (H.toValue h) ! A.src path
        showThumb = foldMap (showImage thumbLink . thumbnailDim) $ dim f
        showFallback path = showImage path $ Dim 100 100

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

flagsToText :: ThreadInfo -> Text
flagsToText ti = 
    if sticky ti    then "(S)" else T.empty <>
    if lock ti      then "(L)" else T.empty <>
    if autosage ti  then "(A)" else T.empty <>
    if cycle_ ti    then "(C)" else T.empty

catalogThread :: ThreadHead -> H.Html
catalogThread h = H.div ! A.class_ "catalog-thread" $ do
    H.div ! A.class_ "catalog-thread-link" $ H.a ! A.href threadLink $
        case file $ opPost h of
            Just f -> let thumbLink = H.toValue $ fileThumbLink f in H.img ! A.src thumbLink
            Nothing -> "***"
    H.div ! A.class_ "thread-info" $ do
        --H.span ! class_ "thread-board-link" $ a ! href "/b" $ "/b/"
        H.span ! A.class_ "thread-info-replies" $ H.toHtml  ("R:" ++ (show $ replyCount $ opInfo h))
        H.span ! A.class_ "thread-info-flags" $ H.toHtml $ flagsToText $ opInfo h
    H.div ! A.class_ "catalog-thread-latest-post" $ do
        "L:"
        H.time ! A.datetime (H.toValue threadDate) $ H.toHtml threadDate
    H.div ! A.class_ "catalog-thread-subject" $ H.toHtml postSubject
    H.div ! A.class_ "catalog-thread-comment" $ H.preEscapedToHtml postText
    where
        p = opPost h
        threadLink = H.toValue $ mconcat ["/", show $ number p]
        threadDate = formatTime defaultTimeLocale "%F %T" (lastBump $ opInfo h)
        postEmail = email $ content p
        postSubject = subject $ content p
        postText = (if postEmail == "nofo" then escapeHTML else formatPost) $ text $ content p

threadView :: Thread -> H.Html
threadView (Thread h ps) = commonHtml $ do 
    H.a ! A.id "new-post" ! A.href "#postform" $ "[Reply]"
    H.hr ! A.class_ "invisible"
    postForm $ Just $ number $ opPost h
    H.hr 
    H.div ! A.class_ "content" $ 
        mconcat $ List.intersperse (H.hr ! A.class_ "invisible") $ postView <$> (opPost h:ps)
    H.hr

boardView :: [ThreadHead] -> H.Html
boardView ts = commonHtml $ do
    H.a ! A.id "new-post" ! A.href "#postform" $ "[New thread]"
    H.hr ! A.class_ "invisible"
    postForm Nothing
    H.hr 
    H.div ! A.class_ "content" $ 
        H.div ! A.class_ "catalog-container" $
        mconcat $ List.intersperse (H.hr ! A.class_ "invisible") $ catalogThread <$> ts
    H.hr

-- | Create error page with given text as error text.
errorView :: Text -> H.Html
errorView msg = commonHtml $ do
    H.div ! A.class_ "content" $
        H.div ! A.class_ "container narrow" $ H.text msg