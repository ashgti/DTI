module Language.Perl.Types where

import Data.List
--import Data.Array
import Data.IORef
import qualified Data.Map
import Control.Monad.Error
import Text.Parsec.Error
import System.IO
--import System.IO.Error

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
                 | GlobContext | FunctionContext | ReferenceContext CallContext | PlusContext
                 | OptionalArgs | ImplicitArg
                 | AnyOfContext [CallContext]
    deriving (Show, Eq)

type Prototype = [CallContext]

data PerlVal =
  {- Scalar Values -}
    Scalar String
  {- List Values -}
  | List [PerlVal]
  {- Perl Hash values -}
  | HashTable (Data.Map.Map String PerlVal)
  | Number Integer
  | Float Double
  | String String
  | Block [PerlVal]
  | PrimitiveFunc ([PerlVal] -> ThrowsError PerlVal)
  | Sub { pvSubName      :: String
        , pvSubBody      :: PerlVal
        , pvSubPrototype :: Prototype
        , pvSubClosure   :: Maybe Env
        }
  | Invoke String [PerlVal]
  | IOFunc ([PerlVal] -> IOThrowsError PerlVal)
  | Port Handle
  | Continuation { closure :: Env                       -- Environment of the continuation
                 , currentCont :: (Maybe DeferredCode)  -- Code of current continuation
                 , nextCont :: (Maybe PerlVal)       -- Code to resume after body of cont
                 , extraReturnArgs :: (Maybe [PerlVal]) -- Extra return arguments, to support (values) and (call-with-values)
                 , dynamicWind :: (Maybe [DynamicWinders]) -- Functions injected by (dynamic-wind)
                 }
  | Undef
  | EOF
  | NOP
  | Nil String

-- |Container to hold code that is passed to a continuation for deferred execution
data DeferredCode =
    SchemeBody [PerlVal] | -- ^A block of Scheme code
    HaskellBody {
       contFunction :: (Env -> PerlVal -> PerlVal -> Maybe [PerlVal] -> IOThrowsError PerlVal)
     , contFunctionArgs :: (Maybe [PerlVal]) -- Arguments to the higher-order function
    } -- ^A Haskell function

-- |Container to store information from a dynamic-wind
data DynamicWinders = DynamicWinders {
    before :: PerlVal -- ^Function to execute when resuming continuation within extent of dynamic-wind
  , after :: PerlVal -- ^Function to execute when leaving extent of dynamic-wind
}

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
showVal (Sub { pvSubName = name, pvSubBody = body }) =
  "sub " ++ name ++ " " ++ case body of
      Nil a -> a
      _ -> show body
showVal (Port _) = "GLOB(0x)"
showVal (IOFunc _) = "<IO primitive>"
-- showVal (EvalFunc _) = "<procedure>"

instance Show PerlVal where show = showVal


trapError :: -- forall (m :: * -> *) e.
            (MonadError e m, Show e) =>
             m String -> m String
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
extractValue (Left _) = error "Unexpected error in extractValue; "

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runErrorT (trapError action) >>= return . extractValue

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

--unwordsPerlList :: PerlVal -> String
--unwordsPerlList (List a) = unwordsList a

unwordsList :: [PerlVal] -> String
unwordsList vals = foldr (\w s -> w ++ s) "" (intersperse "," $ map show vals)

--numericContext :: PerlValue -> PerlValue
--numericContext 

--stringContext :: PerlValue -> PerlValue
--stringContext a = a

--listContext :: PerlValue -> PerlValue
--listContext a

-- Make an "empty" continuation that does not contain any code
makeNullContinuation :: Env -> PerlVal
makeNullContinuation env = Continuation env Nothing Nothing Nothing Nothing




