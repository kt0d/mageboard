{-# LANGUAGE OverloadedStrings #-}
module Imageboard.Pages (
    errorView,
    module Imageboard.Pages.Catalog,
    module Imageboard.Pages.Thread,
) where
import Data.Text (Text)
import Text.Blaze.Html5((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Imageboard.Pages.Common
import Imageboard.Pages.Catalog
import Imageboard.Pages.Thread

-- | Create error page with given text as error text.
errorView :: Text -> H.Html
errorView msg = commonHtml [] $ do
    H.div ! A.class_ "content" $
        H.div ! A.class_ "container narrow" $ H.text msg