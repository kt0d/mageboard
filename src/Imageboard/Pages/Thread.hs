{-# LANGUAGE OverloadedStrings #-}
module Imageboard.Pages.Thread (
    threadView
) where
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.List as List
import Text.Blaze.Html5((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Printf (printf)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Imageboard.Types
import Imageboard.Markup
import Imageboard.Pages.Common

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

threadView :: Thread -> H.Html
threadView (Thread h ps) = commonHtml $ do 
    H.a ! A.id "new-post" ! A.href "#postform" $ "[Reply]"
    H.hr ! A.class_ "invisible"
    postForm $ Just $ number $ opPost h
    H.hr 
    H.div ! A.class_ "content" $ 
        mconcat $ List.intersperse (H.hr ! A.class_ "invisible") $ postView <$> (opPost h:ps)
    H.hr