{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Imageboard.Pages.Account (
    loginView,
    loggedInPage,
    changePasswordPage
) where
import Text.Blaze.Html5((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Data.Time (defaultTimeLocale)
import Imageboard.Pages.Common
import Imageboard.Types (AccountInfo(..))

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

loggedInPage :: AccountInfo -> H.Html
loggedInPage AccountInfo{..} = commonHtml [] $ do
    H.div ! A.id "postform" $ H.fieldset $ H.form $ H.table $ H.tbody $ do
        H.tr $ do
            H.th $ "Username"
            H.td $ H.text $ user
        H.tr $ do
            H.th $ "Created on"
            H.td $ H.time ! A.datetime (H.toValue creatDate) $ (H.toHtml creatDate)
        H.tr $ do
            H.th $ "Role"
            H.td $ H.string $ show role
        H.tr $ do
            H.td $ H.input ! A.type_ "submit" ! A.formaction "/logout" ! 
                A.formmethod "post" ! A.value "Log out"
        H.tr $ do
            H.td $ H.input ! A.type_ "submit" ! A.formaction "/changepass" ! 
                A.formmethod "get" ! A.value "Change password"
    
    where  
        creatDate = formatDate defaultTimeLocale accountCreated

changePasswordPage :: H.Html
changePasswordPage = commonHtml [] $ do
    H.div ! A.id "postform" $ H.fieldset $ H.form ! A.action "/changepass" ! A.method "post" $ 
        H.table $ H.tbody $ do
        H.tr $ do
            H.th $ H.label ! A.for "old-password" $ "Old password"
            H.td $ H.input ! A.id "old-password" ! A.name "old-password" ! A.type_ "text" ! A.maxlength "320"
        H.tr $ do
            H.th $ H.label ! A.for "new-password" $ "New password"
            H.td $ H.input ! A.id "new-password" ! A.name "new-password" ! A.type_ "text" ! A.maxlength "320"
        H.tr $ do
            H.th $ H.label ! A.for "change-password" $ "Submit"
            H.td $ H.input ! A.id "change-password" ! A.type_ "submit" ! A.value "Change password" 
