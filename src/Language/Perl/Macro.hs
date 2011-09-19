{-

So... Here are my thoughts on macros in perl.

If I can devise a strategy for a hygienic macro system, I could
potential define parts of the language in the macro system and
not have to worry about those details at a lower level. 

Currently most looping constructs could be implemented as
macros, maybe, and I am trying to figure out if I can implement
some of those other language constructs, such as gotos, given/when,
unless, and a few other parts of the language as macros.

-}

module Language.Perl.Macro
    (
      macroEval
    ) where

import Language.Perl.Types

-- For now, it just returns the code as is, for now.
macroEval :: Env -> PerlVal -> IOThrowsError PerlVal
macroEval _ perl@(_) = return perl