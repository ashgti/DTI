module Language.Perl.Numeric where

import Language.Perl.Types
import Control.Monad.Error

-- - Begin GenUtil - http://repetae.net/computer/haskell/GenUtil.hs
foldlM :: Monad m => (a -> b -> m a) -> a -> [b] -> m a
foldlM f v (x : xs) = (f v x) >>= \ a -> foldlM f a xs
foldlM _ v [] = return v

foldl1M :: Monad m => (a -> a -> m a) -> [a] -> m a
foldl1M f (x : xs) = foldlM f x xs
foldl1M _ _ = error "Unexpected error in foldl1M"
-- end GenUtil

-- numericBinop :: (Integer -> Integer -> Integer) -> [PerlVal] -> ThrowsError PerlVal
-- numericBinop _ singleVal@[_] = throwError $ NumArgs 2 singleVal
-- numericBinop op aparams = mapM unpackNum aparams >>= return . Number . foldl1 op
-- 
-- unpackNum :: PerlVal -> ThrowsError Integer
-- unpackNum (Number n) = return n
-- unpackNum notNum = throwError $ TypeMismatch "number" notNum

numAdd :: [PerlVal] -> ThrowsError PerlVal
numAdd [] = return $ Number 0
numAdd aparams = do
  foldl1M (\ a b -> doAdd =<< (numCast [a, b])) aparams
  where doAdd (List [(Number a), (Number b)]) = return $ Number $ a + b
        doAdd (List [(Float a), (Float b)]) = return $ Float $ a + b
        -- doAdd (List [(Rational a), (Rational b)]) = return $ Rational $ a + b
        -- doAdd (List [(Complex a), (Complex b)]) = return $ Complex $ a + b
        doAdd _ = throwError $ Default "Unexpected error in +"


numCast :: [PerlVal] -> ThrowsError PerlVal
numCast [a@(Number _), b@(Number _)] = return $ List [a, b]
numCast [a@(Float _), b@(Float _)] = return $ List [a, b]
numCast [(Number a), b@(Float _)] = return $ List [Float $ fromInteger a, b]
numCast [a@(Float _), (Number b)] = return $ List [a, Float $ fromInteger b]
numCast [a, b] = case a of
               Number _ -> doThrowError b
               Float _ -> doThrowError b
               _ -> doThrowError a
  where doThrowError num = throwError $ TypeMismatch "number" num
numCast _ = throwError $ Default "Unexpected error in numCast"

