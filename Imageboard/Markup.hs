{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Imageboard.Markup (
    formatPost,
    escapeHTML
) where
import Data.Text (Text)
import Data.Text as T
import Data.Text.Lazy.Builder (Builder, toLazyText)
import qualified Data.Text.Lazy.Builder as B
import Imageboard.Markup.PCRE2(gsub, RegexReplace(..))

-- | Escape predefined XML entities in a text value
--
escapeHTML :: Text -> Text
escapeHTML = T.foldr escape mempty
  where
    escape :: Char -> Text -> Text
    escape '<'  b = "&lt;"   `mappend` b
    escape '>'  b = "&gt;"   `mappend` b
    escape '&'  b = "&amp;"  `mappend` b
    escape '"'  b = "&quot;" `mappend` b
    escape '\'' b = "&#39;"  `mappend` b
    --escape '\n' b = "<br>"   `mappend` b
    escape x    b = T.singleton x `mappend` b

formatPost :: Text -> Text
formatPost = doMarkup .  escapeHTML

doMarkup :: Text -> Text
doMarkup = gsub [
  REReplace "&#39;&#39;&#39;(.+?)&#39;&#39;&#39;"     "<b>$1</b>",
  REReplace "&#39;&#39;(.+?)&#39;&#39;"               "<i>$1</i>",
  REReplace "^(&gt;.*)$"                              "<span class=\"greentext\">$1</span>",
  REReplace "^(&lt;.*)$"                              "<span class=\"pinktext\">$1</span>",
  REReplace "==(.+?)=="                               "<span class=\"redtext\">$1</span>"]
