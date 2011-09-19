module Language.Perl.Variables where
import Language.Perl.Types
import Control.Monad.Error
import Data.IORef

-- |Extend given environment by binding a series of values to a new environment.
extendEnv :: Env -> [((String, String), PerlVal)] -> IO Env
extendEnv envRef abindings = do bindinglist <- mapM addBinding abindings >>= newIORef
                                return $ Environment (Just envRef) bindinglist
 where addBinding ((namespace, name), val) = do ref <- newIORef val
                                                return ((namespace, name), ref)

-- Recursively search environments to find one that contains var
findNamespacedEnv :: Env -> String -> String -> IO (Maybe Env)
findNamespacedEnv envRef namespace var = do
  found <- liftIO $ isNamespacedBound envRef namespace var
  if found
     then return (Just envRef)
     else case parentEnv envRef of
               (Just par) -> findNamespacedEnv par namespace var
               Nothing -> return Nothing

-- |Determine if a variable is bound in the default namespace
isBound :: Env -> String -> IO Bool
isBound envRef var = isNamespacedBound envRef varNamespace var

-- |Determine if a variable is bound in a given namespace
isNamespacedBound :: Env -> String -> String -> IO Bool
isNamespacedBound envRef namespace var = (readIORef $ bindings envRef) >>= return . maybe False (const True) . lookup (namespace, var)

-- TODO: should isNamespacedBound be replaced with this? Probably, but one step at a time...
isNamespacedRecBound :: Env -> String -> String -> IO Bool
isNamespacedRecBound envRef namespace var = do
  env <- findNamespacedEnv envRef namespace var
  case env of
    (Just e) -> isNamespacedBound e namespace var
    Nothing -> return False

-- |Retrieve the value of a variable defined in the default namespace
getVar :: Env -> String -> IOThrowsError PerlVal
getVar envRef var = getNamespacedVar envRef varNamespace var

-- |Retrieve the value of a variable defined in a given namespace
getNamespacedVar :: Env -> String -> String -> IOThrowsError PerlVal
getNamespacedVar envRef
                 namespace
                 var = do binds <- liftIO $ readIORef $ bindings envRef
                          case lookup (namespace, var) binds of
                            (Just a) -> liftIO $ readIORef a
                            Nothing -> case parentEnv envRef of
                                         (Just par) -> getNamespacedVar par namespace var
                                         Nothing -> (throwError $ UnboundVar "Getting an unbound variable" var)


-- |Set a variable in the default namespace
setVar, defineVar :: Env -> String -> PerlVal -> IOThrowsError PerlVal
setVar envRef var value = setNamespacedVar envRef varNamespace var value

-- ^Bind a variable in the default namespace
defineVar envRef var value = defineNamespacedVar envRef varNamespace var value

-- |Set a variable in a given namespace
setNamespacedVar :: Env -> String -> String -> PerlVal -> IOThrowsError PerlVal
setNamespacedVar envRef
                 namespace
                 var value = do env <- liftIO $ readIORef $ bindings envRef
                                case lookup (namespace, var) env of
                                  (Just a) -> do -- vprime <- liftIO $ readIORef a
                                                 liftIO $ writeIORef a value
                                                 return value
                                  Nothing -> case parentEnv envRef of
                                              (Just par) -> setNamespacedVar par namespace var value
                                              Nothing -> throwError $ UnboundVar "Setting an unbound variable: " var

-- |Bind a variable in the given namespace
defineNamespacedVar :: Env -> String -> String -> PerlVal -> IOThrowsError PerlVal
defineNamespacedVar envRef
                    namespace
                    var value = do
  alreadyDefined <- liftIO $ isNamespacedBound envRef namespace var
  if alreadyDefined
    then setNamespacedVar envRef namespace var value >> return value
    else liftIO $ do
       valueRef <- newIORef value
       env <- readIORef $ bindings envRef
       writeIORef (bindings envRef) (((namespace, var), valueRef) : env)
       return value

