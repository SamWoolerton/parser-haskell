module Parser.Evaluate
  ( evaluateFormula,
    extract,
    test,
  )
where

import Data.Either
import Data.List.Split
import Data.Text (Text)
import Types

evaluateFormula = eFormula testExpression

extract :: Show a => Either a Types.Primitive -> String
extract (Right x) = ePrimitive x
extract (Left x) = "Error: " ++ show x

ePrimitive :: Primitive -> String
ePrimitive (DNumber n) = show n
ePrimitive (DText t) = show t
ePrimitive (DBool b) = show b

testExpression = DExpression (Expression [DExPrimitive (Right $ DNumber 5), DExOperator Add, DExPrimitive (Right $ DNumber 7), DExOperator Divide, DExPrimitive (Right $ DNumber 2)])

testPrimitive = DNumber 1

-- for debugging
test = handleOperation

eFormula :: Formula -> Either Text Primitive
eFormula (DExpression (Expression ls)) = eExpression ls

eExpression :: [ExpressionPart] -> Either Text Primitive
eExpression [DExPrimitive h] = h
eExpression (DExPrimitive h : rest) = foldl handleExpression h pairs
  where
    split = chunksOf 2 rest
    pairs = map (\[a, b] -> (a, b)) split
    handleExpression acc (DExOperator op, DExPrimitive p) = handleOperation acc op p
    handleExpression _ _ = Left "Invalid expression"
eExpression _ = Left "Expression can't start with an operator"

-- handleOperation :: (Either Text Primitive) Operator (Either Text Primitive) -> Either Text Primitive
handleOperation _ Divide (Right (DNumber 0)) = Left "Can't divide by 0"
handleOperation (Right (DNumber l)) op (Right (DNumber r)) = Right DNumber <*> applyOperator fn l r
  where
    fn = case op of
      Add -> Right (+)
      Subtract -> Right (-)
      Multiply -> Right (*)
      Divide -> Right (/)
      _ -> Left "Invalid operation"
handleOperation (Right (DBool l)) op (Right (DBool r)) = Right DBool <*> applyOperator fn l r
  where
    fn = case op of
      And -> Right (&&)
      Or -> Right (||)
      _ -> Left "Invalid operation"
handleOperation _ _ _ = Left "Invalid expression"

applyOperator op l r = op <*> Right l <*> Right r
