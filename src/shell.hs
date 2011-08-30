module Main where

import System.Environment
import System.Console.Haskeline

import Language.Perl.Core
import Language.Perl.Parser
import Language.Perl.Types

--runOne :: [String] -> IO ()
--runOne args = args

runRepl :: IO ()
runRepl = 
    --do
    -- env <- 
    -- stdlib <- getDataFileName "stdlib.scm"
    -- env <- primitiveBindings
    -- _ <- evalString env $ "(load \"" ++ stdlib ++ "\")"
    runInputT defaultSettings loop
    where
        loop :: InputT IO ()
        loop = do
            minput <- getInputLine "perl> "
            case minput of
                Nothing -> return ()
                Just "quit" -> return ()
                Just "" -> loop
                Just input -> do outputStrLn (readExpr "iperl" input)
                                 -- result <- liftIO (evalString env input)
                                 -- if (length result) > 0
                                 --    then do outputStrLn result
                                 --         loop
                                 --    else loop
                                 loop


main :: IO ()
main = do args <- getArgs
          if null args then runRepl
                       else if head args `notElem` ["-t", "--test"]
                                then putStrLn "foobar"
                                else do putStrLn "Running some test cases..."
                                        putStrLn $ "1 " ++ (readExpr "test" "3+2")
                                        putStrLn $ "2 " ++ (readExpr "test" "3+3+3")
                                        putStrLn $ "3 " ++ (readExpr "test" "{3+3}; 3+3;")
                                        putStrLn $ "4 " ++ (readExpr "test" "3+$a;")
                                        putStrLn $ "5 " ++ (readExpr "test" "my ($a, $b);")
                                        putStrLn $ "6 " ++ (readExpr "test" "my $a;")
                                        putStrLn $ "7 " ++ (readExpr "test" "my %a;")
                                        putStrLn $ "8 " ++ (readExpr "test" "my &a;")
                                        putStrLn $ "9 " ++ (readExpr "test" "my @a;")
                                        putStrLn $ "10 " ++ (readExpr "test" "print $a;")
                                        putStrLn $ "11 " ++ (readExpr "test" "print($a);")
                                        putStrLn $ "12 " ++ (readExpr "test" "print(1 + 3);")
                                        putStrLn $ "13 " ++ (readExpr "test" "2 == 3")
                                        putStrLn $ "14 " ++ (readExpr "test" "my $a = 3; print $a;")
                                        putStrLn $ "15 " ++ (readExpr "test" "my ($a, $b) = 3, 3;")
                                        putStrLn $ "16 " ++ (readExpr "test" "my ($a, $b) = (3, 3);")
                                        putStrLn $ "17 " ++ (readExpr "test" "($a + 3) + 3;")
                                        putStrLn $ "18 " ++ (readExpr "test" "\"hello\\\"\"")
                                        putStrLn $ "19 " ++ (readExpr "test" "print 3 . 4")
                                        putStrLn $ "20 " ++ (readExpr "test" "print a { 2; };")
                                        putStrLn $ "21 " ++ (readExpr "test" "(); ( ); print(); my $a = ();")
                                        putStrLn $ "22 " ++ (readExpr "test" "#!/usr/local/bin/perl\
\#\
\# composite series of images over a background image\
\#\
\\
\if ($#ARGV != 4) {\
\ print \"usage: compem bg.rgb inbase outbase startNum stopNum\n\";\
\ exit;\
\}\
\\
\$bg = $ARGV[0];\
\$inbase = $ARGV[1];\
\$outbase = $ARGV[2];\
\$start = $ARGV[3];\
\$stop = $ARGV[4];\
\\
\# for each image\
\for ($i=$start; $i <= $stop; $i++) {\
\\
\    # pad numbers\
\    $num = $i;\
\    if($i<10) { $num = \"00$i\"; }\
\    elsif($i<100) { $num = \"0$i\"; }\
\\
\    # call unix command \"over\"\
\    $cmd = \"over $bg $inbase.$num $outbase.$num 0 0\";\
\    print $cmd.\"\\n\";\
\    if(system($cmd)) { print \"over failed\\n\"; }\
\}\
\")

