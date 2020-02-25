module Parser.Formula
  ( parseFormula,
    parseOutcome,
  )
where

import Control.Monad
import Data.Text (Text)
import qualified Data.Text as T
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Types

parseFormula :: Text -> Either Text Formula
parseFormula f = format $ parse pFormula "" f
  where
    format (Left _) = Left "Parse error"
    format (Right a) = Right a

parseOutcome :: Text -> String
parseOutcome f = format $ parseFormula f
  where
    format (Left m) = show ("Error: " ++ show m)
    format (Right p) = show p

pText :: Parser Text
pText = do
  void (char '"')
  str <- T.pack <$> many alphaNumChar
  void (char '"')
  return str

pBool :: Parser Bool
pBool = choice [True <$ string "true", False <$ string "false"]

pNumber :: Parser Double
pNumber = L.signed s p
  where
    s = void $ some space1
    p = choice [try L.float, try $ fromIntegral <$> L.decimal]

pPrimitive :: Parser Primitive
pPrimitive = choice [DBool <$> pBool, DText <$> pText, DNumber <$> pNumber]

pOperator :: Parser Operator
pOperator =
  do
    void $ many space1
    op <-
      choice
        [ Add <$ string "+",
          Subtract <$ string "-",
          Multiply <$ string "*",
          Divide <$ string "/",
          And <$ string "&&",
          Or <$ string "||"
        ]
    void $ many space1
    return op

pExpressionPart :: Parser ExpressionPart
pExpressionPart =
  choice [DExPrimitive <$> pPrimitive, DExOperator <$> pOperator]

pExpression :: Parser Expression
pExpression = do
  ls <- some pExpressionPart
  return $ Expression ls

pFunctionName :: Parser FunctionName
pFunctionName = do
  str <- T.pack <$> many (choice [letterChar, char '_'])
  return $ DFunctionName str

pFunctionArg :: Parser FunctionArg
pFunctionArg = DFnPrimitive <$> pPrimitive

pFunctionArgNext :: Parser FunctionArg
pFunctionArgNext = do
  void (char ',')
  void (some $ char ' ')
  pFunctionArg

pFunctionBody :: Parser [FunctionArg]
pFunctionBody = do
  first <- pFunctionArg
  rest <- some pFunctionArgNext
  return $ first : rest

pFunction :: Parser Function
pFunction = do
  name <- pFunctionName
  void (char '(')
  args <- pFunctionBody
  void (char ')')
  return $ Function name args

pFormula :: Parser Formula
pFormula = do
  f <-
    choice
      [ try $ DFunction <$> pFunction,
        try $ DExpression <$> pExpression,
        try $ DPrimitive <$> pPrimitive
      ]
  void eof
  return f
