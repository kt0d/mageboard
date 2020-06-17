{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Imageboard.Pages (
    homePage,
    errorView,
    module Imageboard.Pages.Catalog,
    module Imageboard.Pages.Thread,
    module Imageboard.Pages.Recent,
    loginView
) where
import Data.Text (Text)
import Text.Blaze.Html5((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Imageboard.Pages.Common
import Imageboard.Pages.Catalog
import Imageboard.Pages.Thread
import Imageboard.Pages.Recent
import Imageboard.Types (Board,BoardInfo(..))
-- | Create error page with given text as error text.
errorView :: Text -> H.Html
errorView msg = commonHtml [] $ do
    H.div ! A.class_ "content" $
        H.div ! A.class_ "container narrow" $ H.text msg

homePage :: [BoardInfo] -> H.Html
homePage bs = commonHtml (map name bs) $ do
    H.h1 "Welcome"
    H.div ! A.class_ "container narrow" $ do
        H.h2 "Boards" 
        H.ul $
            flip foldMap bs $ \BoardInfo{..} -> 
                H.li $ do
                    H.a ! A.href ("/" <> H.toValue name) ! A.title (H.toValue subtitle) $ 
                        H.text $ name <> " - " <> title

loginView :: H.Html
loginView = commonHtml [] $ do
    H.div ! A.id "postform" $ H.fieldset $ H.form ! 
        A.method "post" ! A.action "/login" $ H.table $ H.tbody $ do
        H.tr $ do
            H.th $ H.label ! A.for "username" $ "Username"
            H.td $ H.input ! A.id "username" ! A.name "username" ! A.type_ "text" ! A.maxlength "64" 
        H.tr $ do
            H.th $ H.label ! A.for "Password" $ "Password"
            H.td $ H.input ! A.id "password" ! A.name "password" ! A.type_ "password" ! A.maxlength "64"
        H.tr $ do
            H.td $ H.input ! A.type_ "submit" ! A.value "Log in" 