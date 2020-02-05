module Parser.Formula
  ( parseFormula,
  )
where

import Control.Applicative hiding (many, some)
import Control.Monad
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Types

parseFormula :: String -> Text -> Either ParserError Formula
parseFormula = parse pFormula

pText :: Parser Text
pText =
  do
    void (char '"')
    str <- T.pack <$> many alphaNumChar
    void (char '"')
    return str

pBool :: Parser Bool
pBool = choice [True <$ string "true", False <$ string "false"]

pNumber :: Parser Int
pNumber = L.decimal

pPrimitive :: Parser Primitive
pPrimitive = choice [DBool <$> pBool, DText <$> pText, DNumber <$> pNumber]

pOperator :: Parser Operator
pOperator =
  choice
    [ Add <$ string "+",
      Subtract <$ string "-",
      Multiply <$ string "*",
      Divide <$ string "/",
      And <$ string "&&",
      Or <$ string "||"
    ]

pExpression :: Parser Expression
pExpression = do
  left <- pPrimitive
  void (many space1)
  op <- pOperator
  void (many space1)
  right <- pPrimitive
  return $ Expression left op right

pFormula :: Parser Formula
pFormula = choice
  [
    try $ DExpression <$> pExpression,
    try $ DPrimitive <$> pPrimitive
  ]
