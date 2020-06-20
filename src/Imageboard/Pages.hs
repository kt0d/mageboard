{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Imageboard.Pages (
    homePage,
    errorView,
    module Imageboard.Pages.Catalog,
    module Imageboard.Pages.Thread,
    module Imageboard.Pages.Recent,
    module Imageboard.Pages.Account
) where
import Data.Text (Text)
import Text.Blaze.Html5((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Imageboard.Pages.Common
import Imageboard.Pages.Catalog
import Imageboard.Pages.Thread
import Imageboard.Pages.Recent
import Imageboard.Pages.Account
import Imageboard.Types (BoardInfo(..))

-- | Create error page with given text as error text.
errorView :: Text -> H.Html
errorView msg = commonHtml msg [] $ do
    H.div ! A.class_ "content" $
        H.div ! A.class_ "container narrow" $ H.text msg

-- | Render list of boards and some welcome text.
homePage :: [BoardInfo] -> H.Html
homePage bs = commonHtml "Home page" (map name bs) $ do
    H.h1 "Welcome"
    H.div ! A.class_ "container narrow" $ do
        H.h2 "Boards" 
        H.ul $
            flip foldMap bs $ \BoardInfo{..} -> 
                H.li $ do
                    H.a ! A.href ("/" <> H.toValue name) ! A.title (H.toValue subtitle) $ 
                        H.text $ name <> " - " <> title
