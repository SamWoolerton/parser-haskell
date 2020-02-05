module Parser.Evaluate
  ( evaluateFormula,
  )
where

import Data.Either
import Types

extract (Right x) = show x
-- extract (Left x) = "Error: " ++ x
extract (Left x) = show x

-- evaluateFormula :: Formula -> Text
-- evaluateFormula = Right $ eExpression testExpression
evaluateFormula = eExpression testExpression

testExpression =
  DExpression
    ( Expression
        (DNumber 5)
        Multiply
        ( DExpression
            ( Expression (DNumber 1) Add (DNumber 2)
            )
        )
    )

-- testExpression = DExpression (Expression (DNumber 5) Multiply (DNumber 6))

testPrimitive = DNumber 1

-- eExpression(DNumber l, )
-- eExpression (DExpression (Expression (DNumber l) o (DNumber n))) = DNumber (fn l n)
--   where fn = (+)

eExpression (DExpression (Expression l op r)) = handleExpression l op r
-- TODO: fallback function to Left
eExpression _ = testPrimitive

-- TODO: handle Lefts up front - this means that the rest have to be Rights
-- TODO: handle nested Expressions
handleExpression (DNumber l) op (DNumber r) = DNumber (fn l r)
  where
    fn = case op of
      Add -> (+)
      Subtract -> (-)
      Multiply -> (*)
-- Divide -> (/)
-- TODO: handle divide
-- TODO: fallback case to Left
handleExpression (DBool l) op (DBool r) = DBool (fn l r)
  where
    fn = case op of
      And -> (&&)
      Or -> (||)
-- TODO: fallback case to Left
-- TODO: fallback function to Left
