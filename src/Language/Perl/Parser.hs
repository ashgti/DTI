module Language.Perl.Parser where

import Data.List
import qualified Data.Map as M
import Data.Char (digitToInt)
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language
import Data.Functor.Identity
import qualified Text.Parsec.Token as P
import Debug.Trace

import Language.Perl.Types


data LexicalInformation = LexInfo { currentNamespace :: String
                                  , prototypes :: M.Map String CallContext
                                  }

-- Parser Specification --

perlDef :: LanguageDef LexicalInformation
perlDef
    = emptyDef
    { P.commentLine = "#"
    , P.identStart  = identStart
    , P.identLetter = identLetter
    , P.opStart = oneOf ":!$%&*+./<=>?\\^|-~engl"
    , P.opLetter = oneOf ":!%&*+./<=>?\\^|-~qnet"
    , P.reservedNames = reservedNames
    , P.reservedOpNames = ["::", "..", "=", "->", "..."]
    , P.caseSensitive = True
    }

lexer    = P.makeTokenParser perlDef

identStart        = oneOf "$%@&*"
identSecondLetter = letter <|> oneOf "~!@#$%^&*_)(" <|> digit
identLetter       = letter <|> oneOf "_" <|> digit

reservedNames = [ "sub"
                , "my", "our", "local", "has", "state"
                , "if", "elsif", "else", "unless", "given", "when"
                , "eval", "BEGIN", "END", "INIT", "CHECK", "UNITCHECK"
                , "package", "use", "require", "import"]

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
          ; css <- many identLetter
          ; return (c:cs:css)
          }
      <?> "identifier"

identifier = lexeme $ try $
    do name <- ident
       if isReservedName name
           then unexpected ("reserved word " ++ show name)
           else return name

bareIdent = many1 letter
            <?> "identifier"

bareIdentifier = lexeme $ try $
   do name <- bareIdent
      if isReservedName name
          then unexpected ("reserved word " ++ show name)
          else return name

isReservedName = isReserved theReservedNames

isReserved names name
       = scan names
       where
         scan []       = False
         scan (r:rs)   = case compare r name of
                           LT  -> scan rs
                           EQ  -> True
                           GT  -> False

stringLiteral = lexeme (
                       do str <- between (char '"')
                                         (char '"' <?> "end of string")
                                         (many stringChar)
                          return (foldr (maybe id (:)) "" str)
                   -- <|> do str <- between (char '\'')
                   --                       (char '\'' <?> "end of string")
                   --                       (many stringChar)
                   --        return (foldr (maybe id (:)) "" str)
                   <?> "literal string")


stringChar      =   do{ c <- stringLetter; return (Just c) }
                    <|> stringEscape
                    <?> "string character"

stringLetter    = satisfy (\c -> (c /= '"') && (c /= '\\') && (c > '\026'))

stringEscape    = do{ char '\\'
                    ;     do{ escapeGap  ; return Nothing }
                      <|> do{ escapeEmpty; return Nothing }
                      <|> do{ esc <- escapeCode; return (Just esc) }
                    }

escapeEmpty     = char '&'
escapeGap       = do{ many1 space
                    ; char '\\' <?> "end of string gap"
                    }



-- escape codes
escapeCode      = charEsc <|> charNum <|> charAscii <|> charControl
                <?> "escape code"

charControl     = do{ char '^'
                    ; code <- upper
                    ; return (toEnum (fromEnum code - fromEnum 'A'))
                    }

charNum         = do{ code <- decimal
                              <|> do{ char 'o'; number 8 octDigit }
                              <|> do{ char 'x'; number 16 hexDigit }
                    ; return (toEnum (fromInteger code))
                    }

charEsc         = choice (map parseEsc escMap)
                where
                  parseEsc (c,code)     = do{ char c; return code }

charAscii       = choice (map parseAscii asciiMap)
                where
                  parseAscii (asc,code) = try (do{ string asc; return code })


-- escape code tables
escMap          = zip "abfnrtv\\\"\'" "\a\b\f\n\r\t\v\\\"\'"
asciiMap        = zip (ascii3codes ++ ascii2codes) (ascii3 ++ ascii2)

ascii2codes     = ["BS","HT","LF","VT","FF","CR","SO","SI","EM",
                   "FS","GS","RS","US","SP"]
ascii3codes     = ["NUL","SOH","STX","ETX","EOT","ENQ","ACK","BEL",
                   "DLE","DC1","DC2","DC3","DC4","NAK","SYN","ETB",
                   "CAN","SUB","ESC","DEL"]

ascii2          = "\b\t\n\v\f\r\SO\SI\EM\FS\GS\RS\US "
ascii3          = "\NUL\SOH\STX\ETX\EOT\ENQ\ACK\a\DLE\DC1\DC2\DC3\DC4\NAK\SYN\ETB\CAN\SUB\ESC\DEL"

decimal         = number 10 digit
number base baseDigit
        = do{ digits <- many1 baseDigit
            ; let n = foldl (\x d -> base*x + toInteger (digitToInt d)) 0 digits
            ; seq n (return n)
            }


theReservedNames = sort reservedNames

whiteSpace = P.whiteSpace lexer
lexeme = P.lexeme lexer

naturalOrFloat = P.naturalOrFloat lexer
symbol = P.symbol lexer

reserved = P.reserved lexer
reservedOp = P.reservedOp lexer

commaSep = P.commaSep lexer

parseBlock :: ParsecT String LexicalInformation Identity PerlVal
parseBlock = braces (do stmts <- many parseStmt
                        return $ Block stmts)

-- parseSubPrototype :: ParsecT String LexicalInformation Identity PerlVal
-- parseSubPrototype = do
--     string "sub"
--     whiteSpace
--     ident <- bareIdentifier
--     return $ Sub ident (Nil "prototype") [] Nothing

parseSubWithName :: ParsecT String LexicalInformation Identity PerlVal
parseSubWithName = do
    string "sub"
    whiteSpace
    ident <- bareIdentifier
    whiteSpace
    body <- do semi
               return $ Nil "proto"
            <|> parseBlock
    return $ Sub ident body [] Nothing

parseMyTerm :: ParsecT String LexicalInformation Identity PerlVal
parseMyTerm = do
    string "my"
    whiteSpace
    idents <- try ((do ident <- identifier
                       return [Scalar ident])
                   <|> parens (do ident <- commaSep identifier
                                  return (map Scalar ident))
                   <?> "Identifier or Identifier List")
    assignee <- option [] (do whiteSpace
                              symbol "="
                              whiteSpace
                              option [] (commaSep parseExpr) <|> parens (commaSep parseExpr))
    if null assignee then return $ Invoke "my" idents
                             else return $ Invoke "=" [Invoke "my" idents, List assignee]

parseInvoke :: ParsecT String LexicalInformation Identity PerlVal
parseInvoke = do
    word <- bareIdentifier
    whiteSpace
    args <- option [] arguments
                      <|> parens arguments
    return $ Invoke word args
    where arguments = commaSep parseExpr

parseTerm :: ParsecT String LexicalInformation Identity PerlVal
parseTerm = choice [ parseMyTerm
                   , parseExpr
                   , parseInvoke
                   ]
    

parseExpr :: ParsecT String LexicalInformation Identity PerlVal
parseExpr = expr
            <|> parens expr
            <?> "bad expression expression"
    where expr = buildExpressionParser table term 

table   = [ [binary "->" AssocLeft]
          , [postfix "++", postfix "--"]
          , [binary "**" AssocRight]
          , [prefix "~", prefix "!", prefix "\\", prefix "-", prefix "+" ]
          , [binary "*" AssocLeft, binary "/" AssocLeft, binary "%" AssocLeft, binary "x" AssocLeft ]
          , [binary "+" AssocLeft, binary "-" AssocLeft, binary "." AssocLeft ]
          , [binary "==" AssocNone, binary "eq" AssocNone, binary "~~" AssocNone]
          , [binary ".." AssocNone, binary "..." AssocNone]
          , [binary "=" AssocRight]
          , [binary "," AssocLeft, binary "=>" AssocLeft]
          ]

term = try (do symbol "("
               symbol ")"
               return Undef)
   <|> do ident <- identifier
          return $ Scalar ident
   <|> do x <- stringLiteral
          return $ String x
   <|> do num <- naturalOrFloat
          return $ case num of
              Left val -> Number val
              Right val -> Float val
   <?> "simple expression"

binary  name = Infix (do reservedOp name; return $ \ x y -> Invoke ("Op"++name) [x, y])
prefix  name = Prefix (do reservedOp name; return $ \ x -> Invoke ("Op"++name) [x])
postfix name = Postfix (do reservedOp name; return $ \ x -> Invoke ("Op"++name) [x])

parseStmt :: ParsecT String LexicalInformation Identity PerlVal
parseStmt = choice [ parseBlock
                   , parseTerm
                   , parseSubWithName
                   , do semi; return NOP
                   ] <?> "Failed"

parseGrammar :: ParsecT String LexicalInformation Identity [PerlVal]
parseGrammar = many parseStmt

readExpr :: String -> String -> String
readExpr fromWhere input =
    case runParser parseGrammar initialState fromWhere input of 
          Right val -> "Found: " ++ unlines [show i | i <- val]
          Left  err -> "Err: " ++ show err

initialState :: LexicalInformation
initialState = LexInfo "main" M.empty
