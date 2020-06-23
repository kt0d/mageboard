{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards, PartialTypeSignatures #-}
module Imageboard.Actions.Common (
    Action,
    blaze,
    maybeParam,
    maybeFile,
    checkBoxParam,
    maybetoExcept
) where
import Control.Monad.Except
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text.Lazy as Lazy
import qualified Data.ByteString.Lazy as B
import qualified Network.Wai.Parse as N (FileInfo(..))
import qualified Web.Scotty as S
import Text.Blaze.Html5 (Html)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Imageboard.FileUpload (FileData)

type Action = S.ActionM ()

-- | Send HTML created with blaze-html combinators. 
blaze :: Html -> Action
blaze = S.html . renderHtml  
  
-- | Maybe get a parameter as in 'Web.Scotty.param', return Nothing 
-- if it is not found. 
maybeParam :: S.Parsable a => Lazy.Text -> S.ActionM (Maybe a)
maybeParam p = (Just <$> S.param p) `S.rescue` (const $ return Nothing)

-- | Get a parameter as in 'Web.Scotty.param', returning Left
-- if it is not found.
tryParam :: S.Parsable a => Lazy.Text -> S.ActionM (Either Text a)
tryParam p = (Right <$> S.param p) `S.rescue` (return . Left . Lazy.toStrict)

-- | Get checkbox state from HTML form.
checkBoxParam :: Lazy.Text -> S.ActionM Bool
-- Only checked checkboxes will apear in form data set.
-- <https://www.w3.org/TR/2012/WD-html5-20120329/form-submission.html#constructing-form-data-set>
checkBoxParam = fmap isJust . (maybeParam :: _ -> _ (_ Text))

-- | Maybe get non-zero-length file.
maybeFile :: S.ActionM (Maybe FileData)
maybeFile = listToMaybe <$> filter (not . B.null . N.fileContent) <$> map snd <$> S.files 

-- | Maybe return a value, otherwise throw given error.
maybetoExcept :: Monad m => b -> Maybe a -> ExceptT b m a
maybetoExcept e = maybe (throwError e) return 
