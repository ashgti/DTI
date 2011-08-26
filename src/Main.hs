module Main where

import Monad
import System.Environment
import Control.Monad.Error
import Data.IORef
import Data.List ( sort )
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language
import Data.Functor.Identity
import Data.Map
import Debug.Trace
import qualified Text.Parsec.Token as P
import IO hiding (try)

-- Data Types --

data Env = Environment { parentEnv :: (Maybe Env)
                       , bindings :: (IORef [((String, String), IORef PerlVal)])
                       }

data PerlVal = Scalar String
             | List [PerlVal]
             | HastTable (Data.Map.Map String PerlVal)
             | Number Integer
             | Float Double
             | String String
             | Block [PerlVal]
             -- | PrimitiveFunc ([PerlVal] -> ThrowsError PerlVal)
             -- | Func { params :: [String]
             --        , vararg :: (Maybe String)
             --        , body :: [PerlVal]
             --        , closure :: Env
             --        }
             | Invoke String [PerlVal]
             -- | IOFunc ([PerlVal] -> IOThrowsError PerlVal)
             | Port Handle
             | EOF
             | NOP
             | Nil String
    deriving (Show)

-- |Types of errors that may occur when evaluating Scheme code
data PerlError = NumArgs Integer [PerlVal] -- ^Invalid number of function arguments
               | TypeMismatch String PerlVal -- ^Type error
               | Parser ParseError -- ^Parsing error
               | BadSpecialForm String PerlVal -- ^Invalid special (built-in) form
               | NotFunction String String
               | UnboundVar String String
               | DivideByZero -- ^Divide by Zero error
               | NotImplemented String
               | InternalError String {- ^An internal error within husk; in theory user (Scheme) code
                                      should never allow one of these errors to be triggered. -}
               | Default String -- ^Default error

type ThrowsError = Either PerlError
type IOThrowsError = ErrorT PerlError IO

-- Parser Specification --


perlDef :: LanguageDef ()
perlDef
    = emptyDef
    { P.commentLine = "#"
    , P.identStart  = identStart
    , P.identLetter = identLetter
    , P.opStart = oneOf ":!#$%&*+./<=>?@\\^|-~engl"
    , P.opLetter = oneOf ":!#%&*+./<=>?@\\^|-~qnet"
    , P.reservedNames = reservedNames
    , P.reservedOpNames = ["::", "..", "=", "->", "..."]
    , P.caseSensitive = True
    }

lexer    = P.makeTokenParser perlDef

identStart        = oneOf "$%@&"
identSecondLetter = letter <|> oneOf "~!@#$%^&*_)(" <|> digit
identLetter       = letter <|> oneOf "~!@#$%^&*_" <|> digit

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

-- symbol :: Stream s m Char => ParsecT s u m Char
-- symbol = oneOf "!$%&|*+-/:<=>?@^_~."

-- parseIdentifier :: Stream s m Char => ParsecT s u m PerlVal
-- parseIdentifier = do
--   atom <- identifier
--   if atom == "."
--      then parserZero -- Do not match this form
--      else return $ Scalar atom

-- parseScalar :: ParsecT String () Identity PerlVal
-- parseScalar = do
--     char '$'
--     name <- parseIndirectObject
--     return $ name
-- 
-- parseIndirectObject :: ParsecT String () Identity PerlVal
-- parseIndirectObject = do
--     word <- identifier
--     return $ Scalar word

parseBlock :: ParsecT String () Identity PerlVal
parseBlock = between (symbol "{") 
                     (symbol "}") 
                     (do stmts <- many parseStmt
                         return $ Block stmts)

-- parseMBlock :: ParsecT String () Identity PerlVal
-- parseMBlock = between (symbol "{") (symbol "}") (remember parseStmtSeq)

-- Start a block
-- remember :: ParsecT String () Identity PerlVal
-- remember = StartBlock True
-- 
-- mremember :: ParsecT String () Identity PerlVal
-- mremember = StartBlock False

-- 
-- parseIdent :: ParsecT String () Identity PerlVal
-- parseIdent = Scalar identifier

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

main :: IO ()
main = do args <- getArgs
          if not $ Prelude.null args then putStrLn (readExpr (args !! 0))
              else do
                  putStrLn "Running some test cases..."
                  putStrLn (readExpr "3+2")
                  putStrLn (readExpr "3+3+3")
                  putStrLn (readExpr "{3+3}; 3+3;")
                  putStrLn (readExpr "3+$a;")
                  putStrLn (readExpr "my ($a, $b);")
                  putStrLn (readExpr "my $a;")
                  putStrLn (readExpr "my %a;")
                  putStrLn (readExpr "my &a;")
                  putStrLn (readExpr "my @a;")
                  putStrLn (readExpr "print $a;")
                  putStrLn (readExpr "print($a);")
                  putStrLn (readExpr "print(1 + 3);")
                  putStrLn (readExpr "2 == 3")
                  putStrLn (readExpr "my $a = 3; print $a;")
                  putStrLn (readExpr "my ($a, $b) = 3, 3;")
                  putStrLn (readExpr "my ($a, $b) = (3, 3);")
                  putStrLn (readExpr "($a + 3) + 3;")
                  putStrLn (readExpr "\"hello\\\"\"")
                  putStrLn (readExpr "print 3 . 4")
