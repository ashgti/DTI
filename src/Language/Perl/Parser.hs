module Language.Perl.Parser where

import Data.List
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language
import Data.Functor.Identity
import qualified Text.Parsec.Token as P

import Language.Perl.Types

-- Parser Specification --

perlDef :: LanguageDef ()
perlDef
    = emptyDef
    { P.commentLine = "#"
    , P.identStart  = identStart
    , P.identLetter = identLetter
    , P.opStart = oneOf ":!$%&*+./<=>?@\\^|-~engl"
    , P.opLetter = oneOf ":!%&*+./<=>?@\\^|-~qnet"
    , P.reservedNames = reservedNames
    , P.reservedOpNames = ["::", "..", "=", "->", "..."]
    , P.caseSensitive = True
    }

lexer    = P.makeTokenParser perlDef

identStart        = oneOf "$%@&*"
identSecondLetter = letter <|> oneOf "~!@#$%^&*_)(" <|> digit
identLetter       = letter <|> oneOf "~!@$%^&*_" <|> digit

reservedNames = ["sub", "my", "our", "local", "if", "elsif", "else", "unless", "package", "use"]

parens   = P.parens lexer
braces   = P.braces lexer
angles   = P.angles lexer
brackets = P.brackets lexer

semi  = P.semi lexer
comma = P.comma lexer
arrow = P.symbol lexer "->"
colon = P.colon lexer

ident = do{ c <- identStart
          ; cs <- identSecondLetter
          ; css <- many (identLetter)
          ; return (c:cs:css)
          }
      <?> "identifier"

identifier = lexeme $ try $
    do name <- ident
       if (isReservedName name)
           then unexpected ("reserved word " ++ show name)
           else return name

bareIdent = do{ cs <- identSecondLetter
              ; css <- many (identLetter)
              ; return (cs:css)
              }
          <?> "identifier"

bareIdentifier = lexeme $ try $
   do name <- bareIdent
      if (isReservedName name)
          then unexpected ("reserved word " ++ show name)
          else return name

isReservedName name
       = isReserved theReservedNames name

isReserved names name
       = scan names
       where
         scan []       = False
         scan (r:rs)   = case (compare r name) of
                           LT  -> scan rs
                           EQ  -> True
                           GT  -> False

theReservedNames = sort reservedNames

whiteSpace = P.whiteSpace lexer
lexeme = P.lexeme lexer

stringLiteral = P.stringLiteral lexer

naturalOrFloat = P.naturalOrFloat lexer
symbol = P.symbol lexer

reserved = P.reserved lexer
reservedOp = P.reservedOp lexer

parseBlock :: ParsecT String () Identity PerlVal
parseBlock = between (symbol "{") 
                     (symbol "}") 
                     (do stmts <- many parseStmt
                         return $ Block stmts)

-- parseSub :: ParsecT String () Identity PerlVal
-- parseSub = do
--     string "sub"
--     spaces
--     ident <- identifier
--     spaces
--     body <- parseBlock

parseMyTerm :: ParsecT String () Identity PerlVal
parseMyTerm = do
    string "my"
    spaces
    idents <- try ((do ident <- identifier
                       return [Scalar ident])
                   <|> (between (symbol "(")
                           (symbol ")")
                           (do ident <- identifier `sepBy` (symbol ",")
                               return (Prelude.map Scalar ident)))
                   <?> "Identifier or Identifier List")
    assignee <- option [] (do spaces
                              symbol "="
                              spaces
                              assignments <- option [] commaSep
                                                       <|> between (symbol "(")
                                                                   (symbol ")")
                                                                   commaSep
                              return assignments)
    if Prelude.null assignee then return $ Invoke "my" idents
                             else return $ Invoke "=" [(Invoke "my" idents), List assignee]
    where commaSep = do params <- parseExpr `sepBy` (symbol ",")
                        return params

parseInvoke :: ParsecT String () Identity PerlVal
parseInvoke = do
    word <- bareIdentifier
    spaces
    args <- option [] foo
                      <|> between (symbol "(")
                                  (symbol ")")
                                  foo
    return $ Invoke word args
    where foo = do params <- parseExpr `sepBy` (symbol ",")
                   return params

parseTerm :: ParsecT String () Identity PerlVal
parseTerm = choice [ parseMyTerm
                   , parseExpr
                   , parseInvoke
                   ]
    

parseExpr :: ParsecT String () Identity PerlVal
parseExpr = expr <|> between (symbol "(")
                             (symbol ")")
                             expr
                 <?> "expression"
    where expr = buildExpressionParser table term 

table   = [ [prefix "-", prefix "+" ]
          , [postfix "++", postfix "--"]
          , [binary "*" AssocLeft, binary "/" AssocLeft ]
          , [binary "+" AssocLeft, binary "-" AssocLeft, binary "." AssocLeft ]
          , [binary "==" AssocNone, binary "eq" AssocNone]
          , [binary "=" AssocRight]
          , [binary "," AssocLeft]
          ]

term = parens parseExpr
   <|> do ident <- identifier
          return $ Scalar ident
   <|> do x <- stringLiteral
          return $ String x
   <|> do num <- naturalOrFloat
          return $ case num of
              Left val -> Number val
              Right val -> Float val
   <?> "simple expression"

-- binary :: String -> Assoc -> Operator s u m a
binary  name assoc = Infix (do reservedOp name; return $ \ x y -> Invoke name [x, y]) assoc
prefix  name       = Prefix (do reservedOp name; return $ \ x -> Invoke name [x])
postfix name       = Postfix (do reservedOp name; return $ \ x -> Invoke name [x])

parseStmt :: ParsecT String () Identity PerlVal
parseStmt = choice [ parseBlock
                   , parseTerm
                   , do semi; return NOP
                   ] <?> "Failed"

parseGrammar :: ParsecT String () Identity [PerlVal]
parseGrammar = do
    stmts <- many parseStmt
    return stmts

readExpr :: String -> String
readExpr input = case parse parseGrammar "" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found " ++ show val
