module Main ( main ) where

import Language.Perl.Core
import Language.Perl.Parser
import Language.Perl.Types

import System.Environment
import Data.Maybe (fromMaybe)
import System.Console.Haskeline
import System.Console.GetOpt
import Control.Monad.Error
import IO hiding (try)

data Options = Options
 { optVerbose      :: Bool
 , optShowVersion  :: Bool
 , optExecutString :: Maybe String
 } deriving Show

defaultOptions     = Options
 { optVerbose      = False
 , optShowVersion  = False
 , optExecutString = Nothing
 }

options :: [OptDescr (Options -> Options)]
options =
 [ Option ['V']     ["verbose"]
     (NoArg (\ opts -> opts { optVerbose = True }))
     "chatty output on stderr"
 , Option ['v','?'] ["version"]
     (NoArg (\ opts -> opts { optShowVersion = True }))
     "show version number"
 , Option ['e']     ["ee"]
     (ReqArg (\ f opts -> opts { optExecutString = Just f }) "program")
     "execute a statement"
 ]

dtiOpts :: [String] -> IO (Options, [String])
dtiOpts argv =
   case getOpt Permute options argv of
      (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
      (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
  where header = "Usage: dti [OPTION...] files..."

main :: IO ()
main = do args <- getArgs
          (opts, others) <- dtiOpts args
          case opts of
              (Options _ True _) -> putStrLn "Version string..."
              (Options _ _ (Just input)) -> do
                  env <- primitiveBindings
                  result <- liftIO (evalString env input)
                  putStrLn result
              (Options _ _ _) -> runRepl

runRepl :: IO ()
runRepl = do
    -- stdlib <- getDataFileName "stdlib.scm"
    env <- primitiveBindings
    -- _ <- evalString env $ "(load \"" ++ stdlib ++ "\")"
    runInputT defaultSettings (loop env)
    where
        loop :: Env -> InputT IO ()
        loop env = do
            minput <- getInputLine "> "
            case minput of
                Nothing -> return ()
                Just "quit" -> return ()
                Just "" -> loop env
                Just input -> do result <- liftIO (evalString env input)
                                 if (length result) > 0
                                    then do outputStrLn result
                                            loop env
                                    else loop env

--                        else if head args `notElem` ["-t", "--test"]
--                                 then putStrLn "foobar"
--                                 else do putStrLn "Running some test cases..."
--                                         putStrLn $ "1 " ++ (readExpr "3+2")
--                                         putStrLn $ "2 " ++ (readExpr "3+3+3")
--                                         putStrLn $ "3 " ++ (readExpr "{3+3}; 3+3;")
--                                         putStrLn $ "4 " ++ (readExpr "3+$a;")
--                                         putStrLn $ "5 " ++ (readExpr "my ($a, $b);")
--                                         putStrLn $ "6 " ++ (readExpr "my $a;")
--                                         putStrLn $ "7 " ++ (readExpr "my %a;")
--                                         putStrLn $ "8 " ++ (readExpr "my &a;")
--                                         putStrLn $ "9 " ++ (readExpr "my @a;")
--                                         putStrLn $ "10 " ++ (readExpr "print $a;")
--                                         putStrLn $ "11 " ++ (readExpr "print($a);")
--                                         putStrLn $ "12 " ++ (readExpr "print(1 + 3);")
--                                         putStrLn $ "13 " ++ (readExpr "2 == 3")
--                                         putStrLn $ "14 " ++ (readExpr "my $a = 3; print $a;")
--                                         putStrLn $ "15 " ++ (readExpr "my ($a, $b) = 3, 3;")
--                                         putStrLn $ "16 " ++ (readExpr "my ($a, $b) = (3, 3);")
--                                         putStrLn $ "17 " ++ (readExpr "($a + 3) + 3;")
--                                         putStrLn $ "18 " ++ (readExpr "\"hello\\\"\"")
--                                         putStrLn $ "19 " ++ (readExpr "print 3 . 4")
--                                         putStrLn $ "20 " ++ (readExpr "print a { 2; };")
--                                         putStrLn $ "21 " ++ (readExpr "(); ( ); print(); my $a = ();")
--                                         putStrLn $ "22 " ++ (readExpr "#!/usr/local/bin/perl\
-- \#\
-- \# composite series of images over a background image\
-- \#\
-- \\
-- \if ($#ARGV != 4) {\
-- \ print \"usage: compem bg.rgb inbase outbase startNum stopNum\n\";\
-- \ exit;\
-- \}\
-- \\
-- \$bg = $ARGV[0];\
-- \$inbase = $ARGV[1];\
-- \$outbase = $ARGV[2];\
-- \$start = $ARGV[3];\
-- \$stop = $ARGV[4];\
-- \\
-- \# for each image\
-- \for ($i=$start; $i <= $stop; $i++) {\
-- \\
-- \    # pad numbers\
-- \    $num = $i;\
-- \    if($i<10) { $num = \"00$i\"; }\
-- \    elsif($i<100) { $num = \"0$i\"; }\
-- \\
-- \    # call unix command \"over\"\
-- \    $cmd = \"over $bg $inbase.$num $outbase.$num 0 0\";\
-- \    print $cmd.\"\\n\";\
-- \    if(system($cmd)) { print \"over failed\\n\"; }\
-- \}\
-- \")
-- 
