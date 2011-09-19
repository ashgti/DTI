module Language.Perl.Primitives where

import Language.Perl.Types
import Control.Monad.Error
import Char
import Data.Array
import Data.Unique
import qualified Data.Map
import IO hiding (try)
import System.Directory (doesFileExist)
import System.IO.Error

makePort :: IOMode -> [PerlVal] -> IOThrowsError PerlVal
makePort mode [String filename] = liftM Port $ liftIO $ openFile filename mode
makePort _ [] = throwError $ NumArgs 1 []
makePort _ args@(_ : _) = throwError $ NumArgs 1 args

closePort :: [PerlVal] -> IOThrowsError PerlVal
closePort [Port port] = liftIO $ hClose port >> (return $ Number 1)
closePort _ = return $ Number 0

writeProc func [obj] = writeProc func [obj, Port stdout]
writeProc func [obj, Port port] = do
    output <- liftIO $ try (liftIO $ func port obj)
    case output of
        Left _ -> throwError $ Default "I/O error writing to port"
        Right _ -> return $ Nil ""
writeProc _ other = if length other == 2
                     then throwError $ TypeMismatch "(value port)" $ List other
                     else throwError $ NumArgs 2 other

