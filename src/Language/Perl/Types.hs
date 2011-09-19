module Language.Perl.Types where

import Data.List
--import Data.Array
import Data.IORef
import qualified Data.Map
import Control.Monad.Error
import Text.Parsec.Error
import System.IO
--import System.IO.Error

data Env = Environment { parentEnv :: (Maybe Env)
                       , bindings :: (IORef [((String, String), IORef PerlVal)])
                       } -- lookup via: (namespace, variable)

nullEnv :: IO Env
nullEnv = do nullBindings <- newIORef []
             return $ Environment Nothing nullBindings

-- Internal namespace for macros
macroNamespace :: [Char]
macroNamespace = "macro"

-- Internal namespace for variables
varNamespace :: [Char]
varNamespace = "core"

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
        , pvSubBody      :: [PerlVal]
        , pvSubPrototype :: Prototype
        , pvSubClosure   :: Maybe Env
        }
  | Invoke String [PerlVal]
  | IOFunc ([PerlVal] -> IOThrowsError PerlVal)
  | EvalFunc ([PerlVal] -> IOThrowsError PerlVal)
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
    PerlBody [PerlVal] | -- ^A block of Scheme code
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
      [Nil a] -> a
      _ -> show body
showVal (Port _) = "GLOB(0x)"
showVal (IOFunc _) = "<IO primitive>"
showVal (Continuation _ _ _ _ _) = "<Continuation>"
showVal (EvalFunc _) = "<procedure>"

instance Show PerlVal where show = showVal

instance Ord PerlVal where
  compare (Number a) (Number b) = compare a b
  compare (Float a) (Float b) = compare a b
  compare (String a) (String b) = compare a b
  compare (Scalar a) (Scalar b) = compare a b
  compare a b = compare (show a) (show b)


-- |Compare two 'PerlVal' instances
eqv :: [PerlVal] -> ThrowsError PerlVal
eqv [(Number arg1), (Number arg2)] = if arg1 == arg2 then return $ Number 1
                                                     else return $ Number 0
eqv [(Float arg1), (Float arg2)] = if arg1 == arg2 then return $ Number 1
                                                   else return $ Number 0
eqv [(String arg1), (String arg2)] = if arg1 == arg2 then return $ Number 1
                                                     else return $ Number 0
eqv [(Scalar arg1), (Scalar arg2)] = if arg1 == arg2 then return $ Number 1
                                                     else return $ Number 0
eqv [(HashTable arg1), (HashTable arg2)] =
  eqv [List $ (map (\ (x, y) -> List [String x, y]) $ Data.Map.toAscList arg1),
       List $ (map (\ (x, y) -> List [String x, y]) $ Data.Map.toAscList arg2)]
eqv [l1@(List _), l2@(List _)] = eqvList eqv [l1, l2]
eqv [_, _] = return $ Number 0
eqv badArgList = throwError $ NumArgs 2 badArgList

eqvList :: ([PerlVal] -> ThrowsError PerlVal) -> [PerlVal] -> ThrowsError PerlVal
eqvList eqvFunc [(List arg1), (List arg2)] = return $ Number $ if (length arg1 == length arg2) &&
                                                                  (all eqvPair $ zip arg1 arg2)
                                                                    then 1
                                                                    else 0
    where eqvPair (x1, x2) = case eqvFunc [x1, x2] of
                               Left _ -> False
                               Right (Number val) -> if val == 0 then False
                                                                 else True
                               _ -> False -- OK?
eqvList _ _ = throwError $ Default "Unexpected error in eqvList"

eqVal :: PerlVal -> PerlVal -> Bool
eqVal a b = do
  let result = eqv [a, b]
  case result of
    Left _ -> False
    Right (Number val) -> if val == 1 then True
                                      else False
    _ -> False -- Is this OK?

instance Eq PerlVal where
  x == y = eqVal x y

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

-- Make a continuation that takes a higher-order function (written in Haskell)
makeCPS :: Env -> PerlVal -> (Env -> PerlVal -> PerlVal -> Maybe [PerlVal] -> IOThrowsError PerlVal) -> PerlVal
makeCPS env cont@(Continuation _ _ _ _ dynWind) cps = Continuation env (Just (HaskellBody cps Nothing)) (Just cont) Nothing dynWind
makeCPS env cont cps = Continuation env (Just (HaskellBody cps Nothing)) (Just cont) Nothing Nothing -- This overload just here for completeness; it should never be used

-- Make a continuation that stores a higher-order function and arguments to that function
makeCPSWArgs :: Env -> PerlVal -> (Env -> PerlVal -> PerlVal -> Maybe [PerlVal] -> IOThrowsError PerlVal) -> [PerlVal] -> PerlVal
makeCPSWArgs env cont@(Continuation _ _ _ _ dynWind) cps args = Continuation env (Just (HaskellBody cps (Just args))) (Just cont) Nothing dynWind
makeCPSWArgs env cont cps args = Continuation env (Just (HaskellBody cps (Just args))) (Just cont) Nothing Nothing -- This overload just here for completeness; it should never be used

--numericContext :: PerlValue -> PerlValue
--numericContext 

--stringContext :: PerlValue -> PerlValue
--stringContext a = a

--listContext :: PerlValue -> PerlValue
--listContext a

-- Make an "empty" continuation that does not contain any code
makeNullContinuation :: Env -> PerlVal
makeNullContinuation env = Continuation env Nothing Nothing Nothing Nothing




