{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Imageboard.Markup (
    formatPost,
    escapeHTML
) where
import Data.Text (Text)
import qualified Data.Text as T
import Regex.PCRE2(gsub, RegexReplace(..))

-- | Escape XML entities in a text value.
escapeHTML :: Text -> Text
escapeHTML = T.foldr escape mempty
  where
    escape :: Char -> Text -> Text
    escape '<'  b = "&lt;"   `mappend` b
    escape '>'  b = "&gt;"   `mappend` b
    escape '&'  b = "&amp;"  `mappend` b
    escape '"'  b = "&quot;" `mappend` b
    escape x    b = T.singleton x `mappend` b

-- | Escape HTML and apply formatting to given text.
formatPost :: Text -> Text
formatPost = doMarkup .  escapeHTML

doMarkup :: Text -> Text
doMarkup = gsub [
  REReplace "&gt;&gt;(\\d+)"                          "<a href=\"#postid$1\">$0</a>",
  REReplace "'''(.+?)'''"                             "<b>$1</b>",
  REReplace "''(.+?)''"                               "<i>$1</i>",
  REReplace "__(.+?)__"                               "<u>$1</u>",
  REReplace "~~(.+?)~~"                               "<s>$1</s>",
  REReplace "^(&gt;.*)$"                              "<span class=\"greentext\">$1</span>",
  REReplace "^(&lt;.*)$"                              "<span class=\"pinktext\">$1</span>",
  REReplace "==(.+?)=="                               "<span class=\"redtext\">$1</span>",
  REReplace "\\*\\*(.+?)\\*\\*"                       "<span class=\"spoiler\">$1</span>",
  REReplace "(https?|ftp)://[^\\s/$.?#].[^\\s]*"      "<a href=\"$0\">$0</a>"]
