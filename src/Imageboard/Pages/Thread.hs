{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Imageboard.Pages.Thread (
    threadView,
    postView
) where
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.List as List
import Text.Blaze.Html5((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Printf (printf)
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
fileBox f@(File{..}) = do
    H.div ! A.class_ "post-file-info" $ do
        H.a ! A.target "blank" ! A.href link $ H.text "File"
        space
        H.toHtml $ show ext
        space
        H.toHtml ("(" :: Text)
        H.a ! H.customAttribute "download" "" ! A.href link $ H.text "dl"
        H.toHtml $ ") " `T.append` (sizeFormat size)
        foldMap dimFormat dim
    case ext of
        e | isAudio e -> showAudio
        EPUB -> showFallback "epub.png"
        SWF -> showFallback "swf.png"
        _ -> showThumb
    where
        dimFormat :: Dimensions -> H.Html
        dimFormat (Dim x y) = space >> (H.string $ printf "%dx%d" x y)
        link =  "/media/" <> (H.toValue filename)
        thumbLink = H.toValue $ fileThumbLink f
        showAudio = H.audio ! A.preload "none" ! A.loop "" ! A.controls "" $
            H.source ! A.type_ (if ext == OGG then "audio/ogg" else "audio/mpeg") ! A.src link
        showImage :: H.AttributeValue -> Dimensions -> H.Html
        showImage path (Dim w h) = H.a ! A.type_ "blank" ! A.href link $ 
            H.img ! A.class_ "post-file-thumbnail" ! 
            A.width (H.toValue w) ! A.height (H.toValue h) ! A.src path
        showThumb = foldMap (showImage thumbLink . thumbnailDim) dim
        showFallback path = showImage path $ Dim 100 100

basePostView :: Post -> H.Html -> H.Html
basePostView Post{..} afterNumber = H.div ! A.class_ "post-container" ! A.id postNumber $ do
    H.div ! A.class_ "post"  $ do
        H.div ! A.class_ "post-header" $ do
            H.span ! A.class_ "post-subject" $ H.toHtml $ postSubject
            space
            H.span ! A.class_ "post-name" $ do 
                (if T.null postEmail then id else emailTag) $ H.toHtml $ postAuthor
            space
            H.span ! A.class_ "post-date" $ 
                H.time ! A.datetime (H.toValue postDate) $ (H.toHtml postDate)
            space
            H.span ! A.class_ "post-number" $ do
                H.a ! A.href ("#" <> postNumber) $ "No."
                H.a ! A.href "#postform" $ H.toHtml $ number loc
            space
            afterNumber
        foldMap fileBox file
        H.div ! A.class_ "post-comment" $ H.preEscapedToHtml postText
    H.br
    where  
        emailTag = H.a ! A.class_ "post-email" ! A.href ("mailto:" <> H.toValue postEmail)
        postNumber = "postid" <> (H.toValue $ number loc)
        postDate = formatDate date
        postAuthor = author content
        postEmail = email content
        postSubject = subject content
        postText = (if postEmail == "nofo" then escapeHTML else formatPost) $ text content

postModLinks :: Post -> H.Html
postModLinks Post{..} = do
    foldMap 
            (\File{..} -> do
                let f = H.toValue filename
                H.a ! A.title "Delete file" ! A.href ("/delete-file/" <> f) $ "[F]"
                H.a ! A.title "Unlink file" ! A.href ("/unlink/" <> postAddrVal) $ "[U]")
            file
    H.a ! A.title "Delete post" ! A.href ("/delete/" <> postAddrVal) $ "[D]"
    where  
        postAddrVal = H.toValue $ board loc <> "/" <> (T.pack $ show $ number loc)

threadModLinks :: Post -> H.Html
threadModLinks Post{..} = do
    H.a ! A.title "Toggle sticky" ! A.href ("/sticky/" <> postAddrVal) $ "[S]"
    H.a ! A.title "Toggle lock" ! A.href ("/lock/" <> postAddrVal) $ "[L]"
    H.a ! A.title "Toggle autosage" ! A.href ("/autosage/" <> postAddrVal) $ "[A]"
    H.a ! A.title "Toggle cycle" ! A.href ("/cycle/" <> postAddrVal) $ "[C]"
    where  
        postAddrVal = H.toValue $ board loc <> "/" <> (T.pack $ show $ number loc)

opPostView :: ThreadHead -> H.Html
opPostView ThreadHead{..} = basePostView opPost $ do
    H.span ! A.class_ "thread-flags" $ renderFlags $ flags opInfo
    space
    H.span ! A.class_ "mod-links" $ do
        postModLinks opPost
        threadModLinks opPost

-- | A view for one post, with mod links.
postView :: Post -> H.Html 
postView p = basePostView p $
    H.span ! A.class_ "mod-links" $ postModLinks p

-- | A view that renders whole thread.
threadView :: BoardInfo -> BoardConstraints -> [Board] -> Thread -> H.Html
threadView BoardInfo{..} Constraints{..} bs Thread{..} = commonHtml (threadTitle op) bs $ do 
    if isLocked
        then H.h3 "Board is locked."
        else if lock $ flags $ opInfo $ op 
            then H.h3 "Thread is locked. You cannot reply."
            else do
                H.a ! A.id "new-post" ! A.href "#postform" $ "[Reply]"
                H.hr ! A.class_ "invisible"
                case loc $ opPost op of (PostLocation b n _) -> replyForm b n
    addTopBottom $ H.div ! A.class_ "content" $ do
        opPostView op
        mconcat $ List.intersperse (H.hr ! A.class_ "invisible") $ postView <$> replies
    where threadTitle (ThreadHead Post{..} _) = "/" <> board loc <> "/" <> (T.pack $ show $ number loc)
