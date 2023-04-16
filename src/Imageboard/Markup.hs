{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Imageboard.Markup (
    formatStub,
    formatStubWithRefs
) where
import Imageboard.Types (Board, RefMap, PostStub(..))
import qualified Data.Map.Strict as M
import Data.Text (Text)
import Data.Text.Read (decimal)
import qualified Data.Text as T
import Regex.PCRE2(gsub, RegexReplace(..), gsubWith)

-- | Escape XML entities in a text value.
escapeHTML :: Text -> Text
escapeHTML = T.foldr escape mempty
  where
    escape :: Char -> Text -> Text
    escape '<'  = (<>) "&lt;"
    escape '>'  = (<>) "&gt;"
    escape '&'  = (<>) "&amp;"
    escape '"'  = (<>) "&quot;"
    escape x    = (<>) $ T.singleton x

-- | Escape HTML and apply formatting to given text.
formatStub :: PostStub -> PostStub
formatStub (Stub a e s t) = Stub a e s $ (if e == "nofo" then id else doMarkup) $ escapeHTML t

-- | Escape HTML and apply formatting to given text, add reference links.
formatStubWithRefs :: Board -> RefMap -> PostStub -> PostStub
formatStubWithRefs b refs (Stub a e s t) = Stub a e s newText where
  newText = if e == "nofo" 
    then escapeHTML t
    else doRefs b refs $ doMarkup $ escapeHTML t

doRefs :: Board -> RefMap -> Text -> Text
doRefs b refs = crossMatch . localMatch where
  localMatch = gsubWith "&gt;&gt;(\\d+)" doLocal
  crossMatch = gsubWith "&gt;&gt;&gt;/(\\w+)/(\\d+)" doCrossBoard

  toNum :: Text -> Int
  toNum x = case decimal x of Right (n,_) -> n

  formatLink :: Text -> Text -> Text -> Text -> Text
  formatLink board parent postid innertext = "<a href=\"/" <> board <> "/" <> parent <> "#postid" <> postid <> "\">" <> innertext <> "</a>"

  formatNoLink :: Text -> Text
  formatNoLink innertext = "<s>" <> innertext <> "</s>"

  doLocal :: (Int -> Text) -> Text
  doLocal matches = case refs M.!? (b,toNum refnum) of
    Nothing -> formatNoLink reftext
    Just p -> formatLink b (T.pack $ show p) refnum reftext
    where
      refnum = matches 1
      reftext = matches 0

  doCrossBoard :: (Int -> Text) -> Text
  doCrossBoard matches = case refs M.!? (refboard,toNum refnum) of
    Nothing -> formatNoLink reftext
    Just p -> formatLink refboard (T.pack $ show p) refnum reftext
    where
      reftext = matches 0
      refboard = matches 1
      refnum = matches 2

doMarkup :: Text -> Text
doMarkup = gsub [
  --REReplace "&gt;&gt;(\\d+)"                          "<a href=\"#postid$1\">$0</a>",
  REReplace "'''(.+?)'''"                             "<b>$1</b>",
  REReplace "''(.+?)''"                               "<i>$1</i>",
  REReplace "__(.+?)__"                               "<u>$1</u>",
  REReplace "~~(.+?)~~"                               "<s>$1</s>",
  REReplace "^(&gt;.*)$"                              "<span class=\"greentext\">$1</span>",
  REReplace "^(&lt;.*)$"                              "<span class=\"pinktext\">$1</span>",
  REReplace "==(.+?)=="                               "<span class=\"redtext\">$1</span>",
  REReplace "\\*\\*(.+?)\\*\\*"                       "<span class=\"spoiler\">$1</span>",
  REReplace "(?s-m)\\`\\`\\`(.+?)\\`\\`\\`"           "<code>$1</code>",
  REReplace "(https?|ftp)://[^\\s/$.?#].[^\\s]*"      "<a href=\"$0\" rel=\"noreferrer\">$0</a>"]
