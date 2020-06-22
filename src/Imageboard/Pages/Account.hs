{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Imageboard.Pages.Account (
    loginView,
    loggedInPage,
    adminLoggedInPage,
    changePasswordPage,
    boardModifyPage,
    createBoardPage,
    createAccountPage
) where
import Text.Blaze.Html5((!), (!?))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Imageboard.Pages.Common
import Imageboard.Types (Board, AccountInfo(..), BoardInfo(..), BoardConstraints(..))

-- | A view with form for logging in.
loginView :: H.Html
loginView = commonHtml "Log in" [] $ do
    H.div $ H.fieldset $ H.form ! A.method "post" ! A.action "/login" $ do
        H.table $ H.tbody $ do
            H.tr $ do
                H.th $ H.label ! A.for "username" $ "Username"
                H.td $ H.input ! A.id "username" ! A.name "username" ! A.type_ "text" ! A.maxlength "64" 
            H.tr $ do
                H.th $ H.label ! A.for "Password" $ "Password"
                H.td $ H.input ! A.id "password" ! A.name "password" ! A.type_ "password" ! A.maxlength "64"
        H.input ! A.type_ "submit" ! A.value "Log in" 

-- | A view rendering account information and role specific information.
loggedInPage :: AccountInfo -> [Board] -> H.Html
loggedInPage acc bs = commonHtml "Your account" bs $ commonLoggedIn acc
    
commonLoggedIn :: AccountInfo -> H.Html
commonLoggedIn AccountInfo{..} = H.div $ H.fieldset $ H.form $ do
    H.table ! A.class_ "listdata" $ do
        H.caption "Your account" 
        H.tbody $ do
            H.tr $ do
                H.th $ "Username"
                H.td $ H.text $ user
            H.tr $ do
                H.th $ "Created on"
                H.td $ H.time ! A.datetime (H.toValue creatDate) $ (H.toHtml creatDate)
            H.tr $ do
                H.th $ "Role"
                H.td $ H.string $ show role
    H.br
    H.input ! A.type_ "submit" ! A.formaction "/logout" ! 
            A.formmethod "post" ! A.value "Log out"
    H.br
    H.input ! A.type_ "submit" ! A.formaction "/changepass" ! 
            A.formmethod "get" ! A.value "Change password"
    where  
        creatDate = formatDate accountCreated

adminLoggedInPage :: AccountInfo -> [BoardInfo] -> [AccountInfo] -> H.Html
adminLoggedInPage acc bs as = commonHtml "Your account" (map name bs) $ do
    commonLoggedIn acc
    boardListing bs
    accountListing as

-- | A view with form for changing password.
changePasswordPage :: H.Html
changePasswordPage = commonHtml "Change password" [] $ do
    H.div $ H.fieldset $ H.form ! A.action "/changepass" ! A.method "post" $ 
        H.table $ H.tbody $ do
        H.tr $ do
            H.th $ H.label ! A.for "old-password" $ "Old password"
            H.td $ H.input ! A.id "old-password" ! A.name "old-password" ! A.type_ "password" ! A.maxlength "320"
        H.tr $ do
            H.th $ H.label ! A.for "new-password" $ "New password"
            H.td $ H.input ! A.id "new-password" ! A.name "new-password" ! A.type_ "password" ! A.maxlength "320"
        H.tr $ do
            H.th $ H.label ! A.for "change-password" $ "Submit"
            H.td $ H.input ! A.id "change-password" ! A.type_ "submit" ! A.value "Change password" 


boardListing :: [BoardInfo] -> H.Html
boardListing bs = H.fieldset $ H.form $  do
    H.table ! A.class_ "listdata" $ do
        H.caption "Boards"
        H.thead $ H.tr $ do
            H.th "Name"
            H.th "Title"
            H.th mempty
        H.tbody $ do
            flip foldMap bs $ \BoardInfo{..} -> H.tr $ do
                H.td $ H.text name
                H.td $ H.a ! A.href ("/" <> H.toValue name) ! A.title (H.toValue subtitle) $ 
                    H.text title
                H.td $ H.a ! A.href ("/boardedit/" <> H.toValue name) $ "Modify"
            H.tr $ H.td ! A.colspan "3" $ H.a ! A.href "/newboard" $ "Add new board"

boardEditTable :: BoardInfo -> BoardConstraints -> H.Html
boardEditTable BoardInfo{..} Constraints{..} = H.table $ H.tbody $ do
        toTextRow "name" "Name" name
        toTextRow "title" "Title" title
        toTextRow "subtitle" "Subtitle" subtitle
        toNumberRow "minlen" "Minimum post length" 0 minLen
        toNumberRow "maxlen" "Maximum post length" 0 maxLen
        toNumberRow "maxnewlines" "Maximum newlines" 0 maxNewLines
        toNumberRow "maxreplies" "Maximum replies" 0 maxReplies
        toNumberRow "maxthreads" "Maximum threads" 0 maxThreads
        H.tr $ do
            H.th $ H.label ! A.for "locked" $ "Locked"
            H.td $ H.input ! A.id "locked" ! A.name "locked" ! A.type_ "checkbox" !? (isLocked, A.checked "")
   where
        toTextRow tagName label defValue = H.tr $ do
            H.th $ H.label ! A.for tagName $ label
            H.td $ H.input ! A.id tagName ! A.name tagName ! 
                A.type_ "text" ! A.value (H.toValue defValue)
        toNumberRow :: H.AttributeValue -> H.Html -> Int -> Int -> H.Html
        toNumberRow tagName label minVal defVal = H.tr $ do
            H.th $ H.label ! A.for tagName $ label
            H.td $ H.input ! A.id tagName ! A.name tagName ! 
                A.type_ "number" ! A.min (H.toValue minVal) ! A.value (H.toValue defVal)

defBoardInfo :: BoardInfo
defBoardInfo = BoardInfo "" "" ""

defConstraints :: BoardConstraints
defConstraints = Constraints False 5 250 25 300 100

-- | A view with form for board creation.
createBoardPage :: H.Html
createBoardPage = commonHtml "Create new board" [] $ 
    H.div $ H.fieldset $ H.form ! 
        A.method "post" ! A.action "/newboard" $ do
        boardEditTable defBoardInfo defConstraints
        H.input ! A.type_ "submit" ! A.value "Create board" 

-- | A view with form for board modification.
boardModifyPage :: BoardInfo -> BoardConstraints -> H.Html
boardModifyPage bi@(BoardInfo{..}) bc = commonHtml "Modify existing board" [] $ 
    H.div $ H.fieldset $ H.form ! 
        A.method "post" ! A.action ("/boardedit/" <> H.toValue name) $ do
        boardEditTable bi bc
        H.input ! A.type_ "submit" ! A.value "Modify board" 

createAccountPage :: H.Html
createAccountPage = commonHtml "Create moderator account" [] $ do
    H.div $ H.fieldset $ H.form ! A.action "/create-account" ! A.method "post" $ 
        H.table $ H.tbody $ do
        H.tr $ do
            H.th $ H.label ! A.for "username" $ "Username"
            H.td $ H.input ! A.id "username" ! A.name "username" ! A.type_ "text" ! A.maxlength "64"
        H.tr $ do
            H.th $ H.label ! A.for "password" $ "Password"
            H.td $ H.input ! A.id "password" ! A.name "password" ! A.type_ "password" ! A.maxlength "320"
        H.tr $ do
            H.th $ H.label ! A.for "create" $ "Submit"
            H.td $ H.input ! A.id "create" ! A.type_ "submit" ! A.value "Create account" 

accountListing :: [AccountInfo] -> H.Html
accountListing as = H.fieldset $ H.form $  do
    H.table ! A.class_ "listdata" $ do
        H.caption "Accounts"
        H.thead $ H.tr $ do
            H.th "Username"
            H.th "Role"
            H.th "Created on"
        H.tbody $ do
            flip foldMap as $ \AccountInfo{..} -> H.tr $ do
                H.td $ H.toHtml user
                H.td $ H.toHtml $ show role
                let creatDate = formatDate accountCreated
                H.td $ H.time ! A.datetime (H.toValue creatDate) $ (H.toHtml creatDate)
            H.tr $ H.td ! A.colspan "3" $ H.a ! A.href "/create-account" $ "Create new account"
