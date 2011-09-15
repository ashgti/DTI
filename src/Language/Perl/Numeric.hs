module Language.Perl.Numerical where

import Language.Perl.Types
import Control.Monad.Error

numericBinop :: (Integer -> Integer -> Integer) -> [PerlVal] -> ThrowsError PerlVal
numericBinop _ singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op aparams = mapM unpackNum aparams >>= return . Number . foldl1 op


unpackNum :: PerlVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum notNum = throwError $ TypeMismatch "number" notNum


