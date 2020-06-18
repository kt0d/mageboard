{-# LANGUAGE OverloadedStrings, RecordWildCards, RankNTypes #-}
module Imageboard.Utils (
    blaze,
    maybeParam,
    maybeFile,
    FileData
) where
import Data.Maybe
import qualified Data.Text.Lazy as Lazy
import qualified Data.ByteString.Lazy as B
import qualified Network.Wai.Parse as N (FileInfo(..))
import qualified Web.Scotty as S
import Text.Blaze.Html5 (Html)
import Text.Blaze.Html.Renderer.Text (renderHtml)

-- | Type representing file as received by HTTP server.
type FileData = N.FileInfo B.ByteString

-- | Send HTML created with blaze-html combinators. 
blaze :: Html -> S.ActionM ()
blaze = S.html . renderHtml  
  
maybeParam :: S.Parsable a => Lazy.Text -> S.ActionM (Maybe a)
maybeParam p = (Just <$> S.param p) `S.rescue` (const $ return Nothing)

maybeFile :: S.ActionM (Maybe FileData)
maybeFile = listToMaybe <$> filter (not . B.null . N.fileContent) <$> map snd <$> S.files 