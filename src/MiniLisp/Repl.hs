{- |
Interactive read-eval-print loop.

Stdin is treated as a lazy list of lines (via 'getContents' + 'lines'),
so EOF arrives as the empty list and needs no exception handling.
The loop threads the top-level 'Env' between forms, so @defun@
definitions persist across entries.

Multi-line input works by accumulating lines until the parentheses
balance (see 'gather' and 'balanced').  A form that is still open at
end of line triggers a @...@ continuation prompt; the finished form is
parsed as a whole.
-}
module MiniLisp.Repl
  ( repl
  , replWith
  , balanced
  ) where

import System.IO        (hSetBuffering, stdout, BufferMode(NoBuffering))
import MiniLisp.Types   (Env, emptyEnv)
import MiniLisp.Parser  (parseProgram)
import MiniLisp.Eval    (evalProgram)

-- | Run a REPL starting with an empty environment.
repl :: IO ()
repl = replWith emptyEnv

-- | Run a REPL from a given environment.  Useful for embedding the REPL
--   with pre-seeded bindings (e.g. a library of standard functions).
--
--   'hSetBuffering' is required so the prompt @> @ reaches the terminal
--   /before/ the read blocks; without it, output would sit in the line
--   buffer waiting for a newline that never comes until the user
--   presses Enter, and the prompt would seem to be missing.
replWith :: Env -> IO ()
replWith startEnv = do
  hSetBuffering stdout NoBuffering
  putStrLn "MiniLisp REPL  (Ctrl-D to exit; '...' continues a form)"
  src <- getContents
  loop startEnv (lines src)

-- | The loop.  Each iteration prints a prompt, takes one line of
--   input, gathers continuation lines until the form is balanced, and
--   runs the resulting program against the current environment.
--
--   Because 'getContents' is lazy and 'lines' is lazy, a single
--   iteration forces exactly one line of stdin at a time; the pattern
--   match on @input@ is what blocks for the user's input.  EOF is
--   represented as @[]@, not as an exception — this is why the module
--   needs no 'Control.Exception' import.
--
--   The prompt is printed /before/ pattern-matching @input@, so it
--   appears before the blocking read.  The `putStrLn ""` in the @[]@
--   branch moves the user's shell prompt to a new line after Ctrl-D.
loop :: Env -> [String] -> IO ()
loop env input = do
  putStr "> "
  case input of
    []        -> do putStrLn ""
                    putStrLn "Bye."
    ln : rest -> do
      (form, remaining) <- gather (ln ++ "\n") rest
      process env form remaining

-- | Accumulate continuation lines until parens balance.  Prints a
--   @...@ prompt before each additional line, so the user can tell
--   they're inside a multi-line form.
--
--   On EOF mid-form we return what we have rather than looping forever;
--   the parser will then produce @parse error@ on the incomplete input,
--   which is a better message than hanging.
gather :: String -> [String] -> IO (String, [String])
gather acc rest
  | balanced acc = return (acc, rest)
  | otherwise    = case rest of
      []        -> return (acc, [])
      ln : more -> do
        putStr "... "
        gather (acc ++ ln ++ "\n") more

-- | Parse and evaluate one form against the current environment, print
--   the result, and continue the loop with the (possibly updated)
--   environment.  Errors print @parse error@ or @eval error@ and leave
--   the environment unchanged.
process :: Env -> String -> [String] -> IO ()
process env form rest =
  case parseProgram form of
    Nothing        -> do putStrLn "parse error"; loop env rest
    Just []        -> loop env rest
    Just es        -> case evalProgram env es of
      Nothing        -> do putStrLn "eval error"; loop env rest
      Just (env', v) -> do print v; loop env' rest

-- | @True@ when parens are balanced and no @)@ would drive the depth
--   negative.  Parentheses inside string literals are ignored.
--
--   An unterminated string counts as balanced: strings cannot span
--   lines in this dialect, so returning @True@ lets the parser report
--   the error rather than stranding the user behind a continuation
--   prompt they can't escape.
balanced :: String -> Bool
balanced = go 0 False
  where
    go :: Int -> Bool -> String -> Bool
    go n _    []              = n == 0
    go n True  ('"' : cs)     = go n False cs
    go n True  (_   : cs)     = go n True  cs
    go n False ('"' : cs)     = go n True  cs
    go n False ('(' : cs)     = go (n + 1) False cs
    go n False (')' : cs)
      | n <= 0                = False
      | otherwise             = go (n - 1) False cs
    go n False (_   : cs)     = go n False cs