{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Imageboard.Markup (
    formatPost,
    escapeHTML
) where
import Data.Text (Text)
import Data.Text as T
import Replace.Attoparsec.Text
import Data.Attoparsec.Text as AT
import Data.Text.Lazy.Builder (Builder, toLazyText)
import qualified Data.Text.Lazy.Builder as B
import Debug.Trace

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
    escape x    b = T.singleton x `mappend` b

formatPost :: Text -> Text
formatPost = doMarkup .  escapeHTML

doMarkup :: Text -> Text
doMarkup = id

-- RE.PCRE is broken https://github.com/iconnect/regex/issues/170
-- import Text.RE.Replace
-- import Text.RE.PCRE.Text

-- doMarkup :: Text -> Text
-- --doMarkup text | trace (T.unpack text) False = undefined
-- doMarkup text = trace ("BEGIN\n" ++ T.unpack t ++ "\nEND\n") t
--     where
--     t = ('\n' `T.cons` text `T.snoc` '\n')
--         *=~/ [ed|&#39;&#39;&#39;$(.+?)&#39;&#39;&#39;///<b>$1</b>|]
--         *=~/ [ed|\n(&gt;.*?)\n///<span class="greentext">$1</span>|]
--         *=~/ [ed|\n(&lt;.*?)\n///<span class="pinktext">$1</span>|]
--         *=~/ [ed|\*\*${spoiler}(.*?)\*\*///<span class="spoiler">${spoiler}</span>|]

