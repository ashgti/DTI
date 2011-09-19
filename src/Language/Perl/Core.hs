module Language.Perl.Core where

import Language.Perl.Types
import Language.Perl.Parser
import Language.Perl.Macro
import Language.Perl.Variables
import Language.Perl.Numeric
import Language.Perl.Primitives
import Control.Monad.Error
import IO hiding (try)
import Debug.Trace

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ liftM show $ (liftThrows $ readExpr expr) >>= macroEval env >>= (eval env (makeNullContinuation env))

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

evalPerl :: Env -> PerlVal -> IOThrowsError PerlVal
evalPerl env perl = macroEval env perl >>= (eval env (makeNullContinuation env))

continueEval :: Env -> PerlVal -> PerlVal -> IOThrowsError PerlVal
{- Passing a higher-order function as the continuation; just evaluate it. This is
 - done to enable an 'eval' function to be broken up into multiple sub-functions,
 - so that any of the sub-functions can be passed around as a continuation. 
 -
 - Carry extra args from the current continuation into the next, to support (call-with-values)
 -}
continueEval _
            (Continuation cEnv (Just (HaskellBody func funcArgs))
                               (Just (Continuation cce cnc ccc _ cdynwind))
                                xargs _) -- rather sloppy, should refactor code so this is not necessary
             val = func cEnv (Continuation cce cnc ccc xargs cdynwind) val funcArgs
{-
 - No higher order function, so:
 -
 - If there is Scheme code to evaluate in the function body, we continue to evaluate it.
 -
 - Otherwise, if all code in the function has been executed, we 'unwind' to an outer
 - continuation (if there is one), or we just return the result. Yes technically with
 - CPS you are supposed to keep calling into functions and never return, but in this case
 - when the computation is complete, you have to return something. 
 -}
continueEval _ (Continuation cEnv (Just (PerlBody cBody)) (Just cCont) extraArgs dynWind) val = do
    case cBody of
        [] -> do
          case cCont of
            Continuation nEnv ncCont nnCont _ nDynWind ->
              -- Pass extra args along if last expression of a function, to support (call-with-values)
              continueEval nEnv (Continuation nEnv ncCont nnCont extraArgs nDynWind) val
            _ -> return (val)
        [lv] -> macroEval cEnv lv >>= eval cEnv (Continuation cEnv (Just (PerlBody [])) (Just cCont) Nothing dynWind)
        (lv : lvs) -> macroEval cEnv lv >>= eval cEnv (Continuation cEnv (Just (PerlBody lvs)) (Just cCont) Nothing dynWind)

-- No current continuation, but a next cont is available; call into it
continueEval _ (Continuation cEnv Nothing (Just cCont) _ _) val = continueEval cEnv cCont val

-- There is no continuation code, just return value
continueEval _ (Continuation _ Nothing Nothing _ _) val = return val
continueEval _ _ _ = throwError $ Default "Internal error in continueEval"


eval :: Env -> PerlVal -> PerlVal -> IOThrowsError PerlVal
eval env cont val@(Nil _) = continueEval env cont val
eval env cont val@(Block ops) = do
    if length ops == 0
        then macroEval env (Nil "") >>= eval env cont
        else if length ops == 1
                 then macroEval env (head ops) >>= eval env cont 
                 else macroEval env (head ops) >>= eval env (makeCPSWArgs env cont cpsRest $ tail ops)
     where cpsRest :: Env -> PerlVal -> PerlVal -> Maybe [PerlVal] -> IOThrowsError PerlVal
           cpsRest e c _ args =
             case args of
               Just fArgs -> macroEval e (Block fArgs) >>= eval e c
               Nothing -> throwError $ Default "Unexpected error in begin"
eval env cont val@(Invoke name args) =
    do func <- getVar env name
       if length args == 0
           then apply cont func []
           else apply cont func (mapErrorT (\ arg -> eval env cont arg) args)
       where evaller :: PerlVal -> IOThrowsError PerlVal
             evaller arg = eval env cont arg
eval _ b c = error $ "Failed " ++ show b ++ " : " ++ show c

evalFuncEval [cont@(Continuation _ _ _ _ _), func, List args] = apply cont func args
evalFuncEval (_ : args) = throwError $ NumArgs 2 args
evalFuncEval _ = throwError $ NumArgs 2 []

evalPrimitives = [("eval", evalFuncEval)]

-- Call into a Perl function
apply :: PerlVal -> PerlVal -> [PerlVal] -> IOThrowsError PerlVal
apply _ cont@(Continuation env ccont ncont _ ndynwind) args = do
-- case (trace ("calling into continuation. dynWind = " ++ show ndynwind) ndynwind) of
  case ndynwind of
    -- Call into dynWind.before if it exists...
    Just ([DynamicWinders beforeFunc _]) -> apply (makeCPS env cont cpsApply) beforeFunc []
    _ -> doApply env cont
 where
   cpsApply :: Env -> PerlVal -> PerlVal -> Maybe [PerlVal] -> IOThrowsError PerlVal
   cpsApply e c _ _ = doApply e c
   doApply e c =
      case (toInteger $ length args) of
        0 -> throwError $ NumArgs 1 []
        1 -> continueEval e c $ head args
        _ ->  -- Pass along additional arguments, so they are available to (call-with-values)
             continueEval e (Continuation env ccont ncont (Just $ tail args) ndynwind) $ head args
apply cont (IOFunc func) args = do
  result <- func args
  case cont of
    Continuation cEnv _ _ _ _ -> continueEval cEnv cont result
    _ -> return result
apply cont (EvalFunc func) args = do
    {- An EvalFunc extends the evaluator so it needs access to the current continuation;
    pass it as the first argument. -}
    func (cont : args)
apply cont (PrimitiveFunc func) args = do
  result <- liftThrows $ func args
  case cont of
    Continuation cEnv _ _ _ _ -> continueEval cEnv cont result
    _ -> return result
apply cont (Sub _ abody aparams (Just aclosure)) args =
  if num aparams /= num args
     then throwError $ NumArgs (num aparams) args
     else (liftIO $ extendEnv aclosure $ [((varNamespace, "_"), List args)]) >>= (evalBody abody)
  where num = toInteger . length
        --
        -- Continue evaluation within the body, preserving the outer continuation.
        --
        {- This link was helpful for implementing this, and has a *lot* of other useful information:
        http://icem-www.folkwang-hochschule.de/~finnendahl/cm_kurse/doc/schintro/schintro_73.html#SEC80 -}
        --
        {- What we are doing now is simply not saving a continuation for tail calls. For now this may
        be good enough, although it may need to be enhanced in the future in order to properly
        detect all tail calls. -}
        --
        -- See: http://icem-www.folkwang-hochschule.de/~finnendahl/cm_kurse/doc/schintro/schintro_142.html#SEC294
        --
        evalBody evBody env = case cont of
            Continuation _ (Just (PerlBody cBody)) (Just cCont) _ cDynWind -> if length cBody == 0
                then continueWCont env (evBody) cCont cDynWind
-- else continueWCont env (evBody) cont (trace ("cDynWind = " ++ show cDynWind) cDynWind) -- Might be a problem, not fully optimizing
                else continueWCont env (evBody) cont cDynWind -- Might be a problem, not fully optimizing
            Continuation _ _ _ _ cDynWind -> continueWCont env (evBody) cont cDynWind
            _ -> continueWCont env (evBody) cont Nothing

        -- Shortcut for calling continueEval
        continueWCont cwcEnv cwcBody cwcCont cwcDynWind =
            continueEval cwcEnv (Continuation cwcEnv (Just (PerlBody cwcBody)) (Just cwcCont) Nothing cwcDynWind) $ Nil ""

        -- bindVarArgs arg env = case arg of
        --   Just argName -> liftIO $ extendEnv env [((varNamespace, argName), List $ remainingArgs)]
        --   Nothing -> return env
apply _ func args = throwError $ BadSpecialForm "Unable to evaluate form" $ List (func : args)

{- |Environment containing the primitive forms that are built into the Scheme language. Note that this only includes
forms that are implemented in Haskell; derived forms implemented in Scheme (such as let, list, etc) are available
in the standard library which must be pulled into the environment using (load). -}
primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= (flip extendEnv $ map (domakeFunc IOFunc) ioPrimitives
                                               ++ map (domakeFunc EvalFunc) evalFunctions
                                               ++ map (domakeFunc PrimitiveFunc) primitives)
  where domakeFunc constructor (var, func) = ((varNamespace, var), constructor func)


evalfuncInvoke :: [PerlVal] -> IOThrowsError PerlVal
evalfuncInvoke [cont@(Continuation _ _ _ _ _), func, List args] = apply cont func args
evalfuncInvoke (_ : args) = throwError $ NumArgs 2 args -- Skip over continuation argument
evalfuncInvoke _ = throwError $ NumArgs 2 []


{- Primitive functions that extend the core evaluator -}
evalFunctions :: [(String, [PerlVal] -> IOThrowsError PerlVal)]
evalFunctions = [("&invoke", evalfuncInvoke)
                ]

{- I/O primitives
Primitive functions that execute within the IO monad -}
ioPrimitives :: [(String, [PerlVal] -> IOThrowsError PerlVal)]
ioPrimitives = [("&open", makePort ReadMode)
               ,("&print", writeProc (\ port obj -> case obj of
                     String str -> hPutStr port str
                     _ -> hPutStr port $ show obj))
               ]

{- "Pure" primitive functions -}
primitives :: [(String, [PerlVal] -> ThrowsError PerlVal)]
primitives = [("&+", numAdd)
             ,("&-", numAdd)
             ,("&*", numAdd)
             ,("&/", numAdd)
             ]

