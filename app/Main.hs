module Main (main) where

import Parser.Formula
import Parser.Evaluate

main :: IO ()
-- main = print $ parseFormula "5 * 6"
main = print evaluateFormula
