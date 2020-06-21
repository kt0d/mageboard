{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Imageboard.Pages.Catalog (
    catalogView
) where
import qualified Data.List as List
import Text.Blaze.Html5((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Imageboard.Pages.Common
import Imageboard.Types 
import Imageboard.Markup

catalogThumbnail :: Maybe File -> H.Html
catalogThumbnail (Just f) = 
    let thumbLink = H.toValue $ case ext f of
            e | isAudio e -> "audio.png"
            EPUB -> "epub.png"
            SWF -> "swf.png"
            _ -> fileThumbLink f
    in H.img ! A.class_ "catalog-thumbnail" ! A.src thumbLink
catalogThumbnail Nothing = "***"

catalogThread :: ThreadHead -> H.Html
catalogThread ThreadHead{..} = H.div ! A.class_ "catalog-thread" $ do
    H.div ! A.class_ "catalog-thread-link" $ H.a ! A.href threadLink $
        catalogThumbnail $ file opPost
    H.div ! A.class_ "thread-info" $ do
        --H.span ! class_ "thread-board-link" $ a ! href "/b" $ "/b/"
        H.span ! A.class_ "thread-info-replies" $ H.toHtml $ "R:" ++ (show $ replyCount opInfo)
        H.span ! A.class_ "thread-info-flags" $ H.toHtml $ flagsToText $ flags opInfo
    H.div ! A.class_ "catalog-thread-latest-post" $ do
        "L:"
        H.time ! A.datetime (H.toValue threadDate) $ H.toHtml threadDate
    H.div ! A.class_ "catalog-thread-subject" $ H.toHtml postSubject
    H.div ! A.class_ "catalog-thread-comment" $ H.preEscapedToHtml postText
    where
        threadLink = "/" <> (H.toValue $ board $ loc opPost) <> "/" <> (H.toValue $ number $ loc opPost)
        threadDate = formatDate $ lastBump opInfo
        postEmail = email $ content opPost
        postSubject = subject $ content opPost
        postText = (if postEmail == "nofo" then escapeHTML else formatPost) $ text $ content opPost

-- | Render list of all threads on a board.
catalogView :: [Board] -> Board -> [ThreadHead] -> H.Html
catalogView bs b ts = commonHtml ("/" <> b <> "/ - catalog") bs $ do
    H.a ! A.id "new-post" ! A.href "#postform" $ "[New thread]"
    H.hr ! A.class_ "invisible"
    threadForm b
    addTopBottom $ H.div ! A.class_ "content" $
        H.div ! A.class_ "catalog-container" $
        mconcat $ List.intersperse (H.hr ! A.class_ "invisible") $ catalogThread <$> ts