module Lexer ( whitespace
             , lexeme
             , symbol
             , integer
             , parens
             , identifier
             , reserved
             , reservedOp
             , operator
             , lambdaSymbol
             ) where

import Text.Parsec
import qualified Text.Parsec.Token as P

langdef :: P.LanguageDef ()
langdef = P.LanguageDef
          { P.commentStart = "(*"
          , P.commentEnd = "*)"
          , P.commentLine = "#"
          , P.nestedComments = True
          , P.identStart = letter    <|> oneOf "_:!#$%&?@\\^|~"
          , P.identLetter = alphaNum <|> oneOf "_:!#$%&+?@\\^|~"
          , P.opStart = oneOf "+*-/<=>"
          , P.opLetter = oneOf "="
          , P.reservedNames = ["λ", "if", "then", "else"]
          , P.reservedOpNames = ["+", "*", "-", "/", "<", "<=", "==", ">=", ">"]
          , P.caseSensitive = True
          }

lexer :: P.TokenParser ()
lexer  = P.makeTokenParser langdef

whitespace = P.whiteSpace lexer
lexeme     = P.lexeme lexer
symbol     = P.symbol lexer
integer    = P.integer lexer
parens     = P.parens lexer
identifier = P.identifier lexer
reserved   = P.reserved lexer
reservedOp = P.reservedOp lexer
operator   = P.operator lexer

lambdaSymbol :: Parsec String () String
lambdaSymbol = symbol "λ" <|> symbol "\\"
