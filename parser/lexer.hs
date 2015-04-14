module Lexer where

import Control.Monad.Identity
import Text.Parsec
import qualified Text.Parsec.Token as P

langdef :: P.GenLanguageDef String () Identity
langdef = P.LanguageDef
          { P.commentStart = "(*"
          , P.commentEnd = "*)"
          , P.commentLine = "#"
          , P.nestedComments = True
          , P.identStart = letter    <|> oneOf "_:!#$%&?@\\^|~"
          , P.identLetter = alphaNum <|> oneOf "_:!#$%&+?@\\^|~"
          , P.opStart = oneOf "+*-/<=>"
          , P.opLetter = oneOf "="
          , P.reservedNames = ["λ"]
          , P.reservedOpNames = ["+", "*", "-", "/", "<", "<=", "=", ">=", ">"]
          , P.caseSensitive = True
          }

lexer :: P.TokenParser ()
lexer  = P.makeTokenParser langdef

whitespace = P.whiteSpace lexer
lexeme     = P.lexeme lexer
symbol     = P.symbol lexer
natural    = P.natural lexer
parens     = P.parens lexer
identifier = P.identifier lexer
reserved   = P.reserved lexer
reservedOp = P.reservedOp lexer

lambdaSymbol :: ParsecT String () Identity String
lambdaSymbol = symbol "λ" <|> symbol "\\"
