module Language.Perl.Types where

import Data.List
import Data.Array
import Data.IORef
import qualified Data.Map
import Control.Monad.Error
import Text.Parsec.Error
import System.IO
import System.IO.Error

data Env = Environment { parentEnv :: Maybe Env
                       , bindings :: IORef [((String, String), IORef PerlVal)]
                       } -- lookup via: (namespace, variable)

nullEnv :: IO Env
nullEnv = do nullBindings <- newIORef []
             return $ Environment Nothing nullBindings

-- |Types of errors that may occur when evaluating Scheme code
data PerlError =
    NumArgs Integer [PerlVal] -- ^Invalid number of function arguments
  | TypeMismatch String PerlVal -- ^Type error
  | Parser ParseError -- ^Parsing error
  | BadSpecialForm String PerlVal -- ^Invalid special (built-in) form
  | NotSub String String
  | UnboundVar String String
  | DivideByZero -- ^Divide by Zero error
  | NotImplemented String
  | InternalError String {- ^An internal error within husk; in theory user (Scheme) code
                         should never allow one of these errors to be triggered. -}
  | Default String -- ^Default error

-- |Create a textual description for a 'PerlError'
showError :: PerlError -> String
showError (NumArgs expected found) = "Expected " ++ show expected
                                  ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                  ++ ", found " ++ show found
showError (Parser parseErr) = "Parse error at " ++ ": " ++ show parseErr
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotSub message func) = message ++ ": " ++ show func
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (DivideByZero) = "Division by zero"
showError (NotImplemented message) = "Not implemented: " ++ message
showError (InternalError message) = "An internal error occurred: " ++ message
showError (Default message) = "Error: " ++ message

instance Show PerlError where show = showError
instance Error PerlError where 
    noMsg = Default "An error has occurred"
    strMsg = Default

type ThrowsError = Either PerlError
type IOThrowsError = ErrorT PerlError IO

data CallContext = ConstContext | VoidContext | ListContext | ScalarContext
                 | GlobContext | FunctionContext | ReferenceContext | PlusContext
                 | AnyOfContext [CallContext]
    deriving (Show, Eq)

type Prototype = [CallContext]

data PerlVal =
  {- Scalar Values -}
    Scalar String
  {- List Values -}
  | List [PerlVal]
  | HashTable (Data.Map.Map String PerlVal)
  | Number Integer
  | Float Double
  | String String
  | Block [PerlVal]
  | PrimitiveFunc ([PerlVal] -> ThrowsError PerlVal)
  | Sub { name      :: String
        , body      :: PerlVal
        , prototype :: Prototype
        , closure   :: Maybe Env
        }
  | Invoke String PerlVal
  | IOFunc ([PerlVal] -> IOThrowsError PerlVal)
  | Port Handle
  | Undef
  | EOF
  | NOP
  | Nil String

showVal :: PerlVal -> String
showVal (Nil _) = ""
showVal (EOF) = "#!EOF"
showVal (NOP) = ";"
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Number contents) = show contents
showVal (Float contents) = show contents
showVal (Scalar name) = name
showVal (Undef) = "undef"
showVal (Invoke name args) = "&" ++ name ++ "(" ++ unwordsList args ++ ")"
showVal (Block contents) = "{"++ unwordsList contents ++"}"
showVal (List contents) = "LIST(" ++ unwordsList contents ++ ")"
showVal (HashTable _) = "HASH(0x)"
showVal (PrimitiveFunc _) = "<primitive>"
showVal (Sub { name = name, body = body }) =
  "sub " ++ name ++ " " ++ case body of
      Nil a -> a
      _ -> show body
showVal (Port _) = "GLOB(0x)"
showVal (IOFunc _) = "<IO primitive>"
-- showVal (EvalFunc _) = "<procedure>"

instance Show PerlVal where show = showVal

unwordsList :: [PerlVal] -> String
unwordsList vals = foldr (\w s -> w ++ s) "" (intersperse "," $ map show vals)

--numericContext :: PerlValue -> PerlValue
--numericContext 

--stringContext :: PerlValue -> PerlValue
--stringContext a = a

--listContext :: PerlValue -> PerlValue
--listContext a
