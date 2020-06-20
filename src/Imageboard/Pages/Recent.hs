{-# LANGUAGE OverloadedStrings #-}
module Imageboard.Pages.Recent (
    recentView
) where
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.List as List
import Text.Blaze.Html5((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Data.Time (formatTime, defaultTimeLocale)
import Imageboard.Types
import Imageboard.Pages.Common
import Imageboard.Pages.Thread (postView)

linkToPost :: PostLocation -> Text
linkToPost (PostLocation b n Nothing) = "/" <> b <> "/" <> (T.pack $ show n)
linkToPost (PostLocation b _ (Just p)) = "/" <> b <> "/" <> (T.pack $ show p)

recentPostView :: Post -> H.Html
recentPostView p = do
    H.a ! A.class_ "recent-threadlink" ! A.href (H.toValue link) $ H.text link
    postView p
    where link = linkToPost $ loc p

recentView :: [Board] -> [Post] -> H.Html
recentView bs ps = commonHtml "Recent posts" bs $ do 
    addTopBottom $ H.div ! A.class_ "content" $
        mconcat $ List.intersperse (H.hr ! A.class_ "invisible") $ map recentPostView ps
