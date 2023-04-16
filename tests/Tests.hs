{-# LANGUAGE OverloadedStrings #-}

module Main (main) where
import System.Exit
import Test.HUnit
import Regex.PCRE2
import Data.Text as T

zazolc      = "Zażółć gęślą jaźń."
revZazolc   = "Zażółć ąlśęg jaźń."
upperZazolc = "ZAŻÓŁĆ GĘŚLĄ JAŹŃ."
lowerZazolc = "zażółć gęślą jaźń."
emptyZazolc = "                 ."
xZazolc     = "XXXXXX XXXXX XXXX."

tests :: Test
tests = test [
        "b"         ~=? gsub [REReplace "a" "b"] "a"
    ,   "bbc"       ~=? gsub [REReplace "a" "b"] "abc"
    ,   "aaźźććxx"  ~=? gsub [REReplace "a" "aa", REReplace "b" "źź", REReplace "c" "ććxx"] "abc"
    ,   "cbacba"    ~=? gsubWith "abc" (T.reverse . ($ 0)) "abcabc"
    ,   "XX"        ~=? gsubWith "abc" (const "X") "abcabc"
    ,   "ABC"       ~=? gsubWith "\\w" (T.toUpper . ($ 0)) "abc"
    ,   revZazolc   ~=? gsubWith "gęślą" (T.reverse . ($ 0)) zazolc
    ,   upperZazolc ~=? gsubWith "\\pL" (T.toUpper . ($ 0)) zazolc
    ,   lowerZazolc ~=? gsubWith "\\pL" (T.toLower . ($ 0)) zazolc
    ,   emptyZazolc ~=? gsubWith "\\pL" (const " ") zazolc
    ,   xZazolc     ~=? gsubWith "\\pL" (const "X") zazolc
    ,   ""          ~=? gsubWith zazolc (const "") zazolc
    ]

main :: IO ()
main = do
    counts2 <- runTestTT tests
    if (errors counts2 + failures counts2 == 0)
        then exitSuccess
        else exitFailure
