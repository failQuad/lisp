{- |
Non-interactive entry point: parse and evaluate a whole program, then
show the final value.  Used by both the @minilisp@ executable (when
not in REPL mode) and by the test suite.
-}
module MiniLisp.Run
  ( run, newlineIfNonEmpty
  ) where

import MiniLisp.Types  (Env, emptyEnv)
import MiniLisp.Parser (parseProgram)
import MiniLisp.Eval   (evalProgram)

-- | Parse and evaluate @src@, returning the printed result or one of
--   the strings @\"parse error\"@ / @\"eval error\"@.
run :: String -> String
run src = case parseProgram src of
  Nothing -> "parse error"
  Just [] -> ""
  Just es -> case evalProgram (emptyEnv :: Env) es of
    Nothing     -> "eval error"
    Just (_, v) -> show v

-- | Append a newline unless the input is empty.  Handy for CLI output.
newlineIfNonEmpty :: String -> String
newlineIfNonEmpty s = if null s then s else s ++ "\n"