module Main (main) where

import Parser.Evaluate
import Parser.Formula

main :: IO ()
-- main = print $ parseOutcome "5.6 * 6"
main = print $ parseOutcome "test_fn(4, 2, 1)"
-- main = print $ extract evaluateFormula
