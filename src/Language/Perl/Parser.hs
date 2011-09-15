module Language.Perl.Parser where

import Data.List
import qualified Data.Map as M
import Data.Char (digitToInt)
import Data.Maybe
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

{- Perl Language definition, its helpful for some token parsing components. -}
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

lexer :: P.GenTokenParser String LexicalInformation Identity
lexer    = P.makeTokenParser perlDef

identStart        = oneOf "$%@&*"
identSecondLetter = letter <|> oneOf "~!@#$%^&*_)(" <|> digit
identLetter       = letter <|> oneOf "_" <|> digit

reservedNames = [ "sub"
                , "my", "our", "local", "has", "state"
                , "if", "elsif", "else", "unless", "given", "when"
                , "eval", "BEGIN", "END", "INIT", "CHECK", "UNITCHECK"
                , "package", "use", "require", "import"
                , "macro", "quasi"]

parens   = P.parens lexer
braces   = P.braces lexer
angles   = P.angles lexer
brackets = P.brackets lexer

semi  = P.semi lexer
comma = P.comma lexer
colon = P.colon lexer
arrow = P.symbol lexer "->"
fatArrow = symbol "=>"


-- Copy Pasta from Text.Parsec.Language because it was really close
-- to what I needed, but still slightly off.
ident = do{ c <- identStart
          ; cs <- identSecondLetter
          ; css <- many identLetter
          ; return (c:cs:css)
          }
      <?> "identifier"

{- Parse an Identifier, this means a sigil + a name. -}
identifier = lexeme $ try $
    do name <- ident
       if isReservedName name
           then unexpected ("reserved word " ++ show name)
           else return name

bareIdent = many1 letter
            <?> "identifier"

{- Parse a bare identifier, this is a bare word such as print or foo -}
bareIdentifier = lexeme $ try $
   do name <- bareIdent
      if isReservedName name
          then unexpected ("reserved word " ++ show name)
          else return name

{- A list of reserved names. -}
isReservedName = isReserved theReservedNames

isReserved names name
       = scan names
       where
         scan []       = False
         scan (r:rs)   = case compare r name of
                           LT  -> scan rs
                           EQ  -> True
                           GT  -> False

{- Parse a string literal. -}
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
lexeme     = P.lexeme lexer
symbol     = P.symbol lexer

naturalOrFloat = P.naturalOrFloat lexer

reserved   = P.reserved lexer
reservedOp = P.reservedOp lexer

commaSep  = P.commaSep lexer
commaSep1 = P.commaSep1 lexer
commaOrFatArrowSep1 p = sepBy1 p (comma <|> fatArrow)

{- Parses a block, basically any statements between { and }. -}
parseBlock :: ParsecT String LexicalInformation Identity PerlVal
parseBlock = braces (do stmts <- many parseStmt
                        return $ Block stmts)

stringToPrototype :: [Char] -> Prototype
stringToPrototype ('\\':'[':xs) = [AnyOfContext $ stringToPrototype left] ++ stringToPrototype (tail right)
                                  where (left, right) = break (==']') xs
stringToPrototype ('\\':x:xs) = [ReferenceContext $ head $ stringToPrototype [x]] ++ stringToPrototype xs
stringToPrototype ('$':xs) = [ScalarContext] ++ stringToPrototype xs
stringToPrototype ('@':xs) = [ListContext] ++ stringToPrototype xs
stringToPrototype ('%':xs) = [ListContext] ++ stringToPrototype xs
stringToPrototype (';':xs) = [OptionalArgs] ++ stringToPrototype xs
stringToPrototype (')':xs) = [ConstContext] ++ stringToPrototype xs
stringToPrototype ('_':xs) = [ImplicitArg] ++ stringToPrototype xs
stringToPrototype ('[':xs) = [] ++ stringToPrototype xs
stringToPrototype (']':xs) = [] ++ stringToPrototype xs
stringToPrototype _ = []

{- Parse an empty set of Parens, just a shortcut function since I write this a lot. -}
emptyParens = symbol "(" >> symbol ")"

{- Parse a sub prototype, this is independent of the rest of the sub definition. -}
parseSubPrototype :: ParsecT String LexicalInformation Identity [Char]
parseSubPrototype = try emptyParens <|> (try $ parens ((many $ oneOf "&@%$\\*;+_ []")
                                                        <?> "Bad prototype")) 
                                    <|> do return ""

{- Parse subs with names. This will not parse an anonymous sub. -}
parseSubWithName :: ParsecT String LexicalInformation Identity PerlVal
parseSubWithName = do
    string "sub"
    whiteSpace
    subName <- bareIdentifier
    whiteSpace

    subProto <- parseSubPrototype
    -- TODO(ash_gti): Prototpes, see op.c:9089
    subBody <- (do semi; return $ Nil "..."
            <|> parseBlock)
    -- A record of the subs with a recording of their call context
    modifyState (\st -> LexInfo (currentNamespace st)
                                (M.insert (currentNamespace st ++ "::" ++ subName)
                                          (VoidContext)
                                          (prototypes st)))
    trace (show subProto) $ trace (show $ stringToPrototype subProto) return $ Sub subName subBody (stringToPrototype subProto) Nothing

-- TODO(ash_gti): Add state, local, our, and possibly has
{- Parse a "my" term. -}
parseMyTerm :: ParsecT String LexicalInformation Identity PerlVal
parseMyTerm = do
    string "my"
    whiteSpace
    myIdents <- try ((do singleIdent <- identifier
                         return [Scalar singleIdent])
                     <|> parens (do manyIdents <- commaSep identifier
                                    return (map Scalar manyIdents))
                     <?> "Identifier or Identifier List")
    assignee <- optionMaybe (do whiteSpace
                                symbol "="
                                whiteSpace
                                parseExpr)
    if isNothing assignee then return $ Invoke "my" myIdents
                          else return $ Invoke "=" [ Invoke "my" myIdents
                                                   , fromJust assignee
                                                   ]

{- Parse a potential sub invocation. -}
parseInvoke :: ParsecT String LexicalInformation Identity PerlVal
parseInvoke = do
    word <- bareIdentifier
    whiteSpace
    args <- option [] (do a <- parseExpr
                          return $ case a of
                              List values -> trace "list" values
                              _ -> trace "not list" [a])
    return $ Invoke word args

-- TODO(ashgti): check if this is the right terminology.
{- Parse a a term, or at least what I am calling a term. -}
parseTerm :: ParsecT String LexicalInformation Identity PerlVal
parseTerm = choice [ parseMyTerm
                   , parseExpr
                   , parseInvoke
                   ]

parseExpr :: ParsecT String LexicalInformation Identity PerlVal
parseExpr = (try $ parens (do a <- commaOrFatArrowSep1 expr
                              return $ List a))
        <|> (try $ parens expr)
        <|> (try $ do a <- commaOrFatArrowSep1 expr
                      return $ List $ trace " aa " a)
        <|> (try $ trace "yup" expr)
        <?> "bad expression expression"
    where expr = buildExpressionParser table term 

-- TODO(ash_gti): add the rest of the ops to the table.
table   = [ [binary "->" AssocLeft]
          , [postfix "++", postfix "--"]
          , [binary "**" AssocRight]
          , [prefix "~", prefix "!", prefix "\\", prefix "-", prefix "+" ]
          , [binary "*" AssocLeft, binary "/" AssocLeft, binary "%" AssocLeft, binary "x" AssocLeft ]
          , [binary "+" AssocLeft, binary "-" AssocLeft, binary "." AssocLeft ]
          , [binary "==" AssocNone, binary "eq" AssocNone, binary "~~" AssocNone]
          , [binary ".." AssocNone, binary "..." AssocNone]
          , [binary "=" AssocRight]
          -- , [binary "," AssocLeft, binary "=>" AssocLeft]
          ]

term = try (do symbol "("
               symbol ")"
               return Undef)
   <|> (trace "trying prototypes" try (do st <- getState
                                          ident <- bareIdentifier
                                          if M.lookup ident (prototypes st) == Just ConstContext
                                               then return $ Invoke ident []
                                               else fail "Not found"))
   <|> do scalarIdent <- identifier
          return $ Scalar scalarIdent
   <|> do x <- stringLiteral
          return $ String x
   <|> do num <- naturalOrFloat
          return $ case num of
              Left val -> Number val
              Right val -> Float val
   <|> try (do spaces
               char ','
               return Undef)

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
parseGrammar = do whiteSpace
                  many parseStmt

readExpr :: String -> String -> String
readExpr fromWhere input =
    case runParser parseGrammar initialState fromWhere input of 
          Right val -> "Found: " ++ unlines [show i | i <- val]
          Left  err -> "Err: " ++ show err

initialState :: LexicalInformation
initialState = LexInfo "main" M.empty

