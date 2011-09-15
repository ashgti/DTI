module Language.Perl.Core where

import Language.Perl.Types
import Language.Perl.Parser
import Language.Perl.Macro
import Control.Monad.Error

listContents :: PerlVal -> [PerlVal]
listContents (List a) = a
listContents _ = error "Not a list..."

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ liftM show $ (liftThrows $ readExpr expr) >>= macroEval env >>= (eval env (makeNullContinuation env)) 

evalAndPrint :: Env -> String -> IO ()
evalAndPrint = putStrLn . evalString

evalPerl :: Env -> PerlVal -> IOThrowsError PerlVal
evalPerl env perl = macroEval env perl >>= (eval env (makeNullContinuation env))

continueEval :: Env -> PerlVal -> PerlVal -> IOThrowsError Perlval

eval :: Env -> PerlVal -> PerlVal -> IOThrowsError PerlVal
eval env cont val@(Nil _) = continueEval env cont val

evalFuncEval [cont@(Continuation _ _ _ _ _), func, List args] = apply cont func args
evalFuncEval (_ : args) = throwError $ NumArgs 2 args
evalFuncEval _ = throwError $ NumArgs 2 []

evalPrimitives = [("eval", evalFuncEval)]

{- "Pure" primitive functions -}
primitives :: [(String, [PerlVal] -> ThrowsError PerlVal)]
primitives = [("+", numAdd),
              ("-", numSub),
              ("*", numMul),
              ("/", numDiv)]

