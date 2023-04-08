{-# LANGUAGE OverloadedStrings #-}

module Main (main) where
import System.Exit
import Test.HUnit
import Regex.PCRE2
import Data.Text as T

tests :: Test
tests = test [
    "gsub s/a/b" ~: "bbc" ~=? gsub [REReplace "a" "b"] "abc",
    "gsubWith" ~: "cbacba" ~=? gsubWith "abcabc" "abc" (T.reverse . ($ 0))
    ]

main :: IO ()
main = do
    counts2 <- runTestTT tests
    if (errors counts2 + failures counts2 == 0)
        then exitSuccess
        else exitFailure
