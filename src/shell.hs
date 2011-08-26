module Main where

-- import Monad
-- import System.Environment
-- import Control.Monad.Error
-- import Data.IORef
-- import Data.List ( sort )
-- import Text.Parsec
-- import Text.Parsec.Expr
-- import Text.Parsec.Language
-- import Data.Functor.Identity
-- import Data.Map
-- import Debug.Trace
-- import qualified Text.Parsec.Token as P
-- import IO hiding (try)

import System.Environment

import Language.Perl.Core
import Language.Perl.Parser
import Language.Perl.Types

main :: IO ()
main = do args <- getArgs
          if not $ Prelude.null args then putStrLn (readExpr (args !! 0))
              else do
                  putStrLn "Running some test cases..."
                  putStrLn (readExpr "3+2")
                  putStrLn (readExpr "3+3+3")
                  putStrLn (readExpr "{3+3}; 3+3;")
                  putStrLn (readExpr "3+$a;")
                  putStrLn (readExpr "my ($a, $b);")
                  putStrLn (readExpr "my $a;")
                  putStrLn (readExpr "my %a;")
                  putStrLn (readExpr "my &a;")
                  putStrLn (readExpr "my @a;")
                  putStrLn (readExpr "print $a;")
                  putStrLn (readExpr "print($a);")
                  putStrLn (readExpr "print(1 + 3);")
                  putStrLn (readExpr "2 == 3")
                  putStrLn (readExpr "my $a = 3; print $a;")
                  putStrLn (readExpr "my ($a, $b) = 3, 3;")
                  putStrLn (readExpr "my ($a, $b) = (3, 3);")
                  putStrLn (readExpr "($a + 3) + 3;")
                  putStrLn (readExpr "\"hello\\\"\"")
                  putStrLn (readExpr "print 3 . 4")
                  putStrLn (readExpr "print a { 2; };")
                  putStrLn (readExpr "#!/usr/local/bin/perl\
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

