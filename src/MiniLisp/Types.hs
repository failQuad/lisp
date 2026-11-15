{- |
Core value and environment types.

'Value' doubles as the reader's output and the evaluator's runtime
representation.  'Env' is a simple association list; every operation is
pure and returns a new 'Env' rather than mutating in place.

Frame model: binding adds a new pair to the front of the list.  A
@let@ or closure application extends the front with its locals, and
@envDrop@ removes them again when the frame exits.  Lookup walks the
list front-to-back, so inner bindings shadow outer ones.
-}
module MiniLisp.Types
  ( Value(..)
  , Env(..)
  , emptyEnv, envLookup, envCons, envPrepend, envDrop, envSet
  , consValue
  ) where

-- | Runtime values.
data Value
  = VNum     Integer              -- ^ integer
  | VFloat   Double               -- ^ floating-point
  | VStr     String               -- ^ string literal
  | VSym     String               -- ^ symbol (variable / function name)
  | VList    [Value]              -- ^ proper list; @VList []@ is @nil@
  | VPair    Value Value          -- ^ dotted pair (improper list tail)
  | VPrim    String               -- ^ built-in primitive
  | VClosure [String] [Value] Env -- ^ user-defined function;
                                  --   captures its defining environment

-- | Lexical environment: an association list of name -> value bindings,
--   newest first.
newtype Env = Env [(String, Value)]

emptyEnv :: Env
emptyEnv = Env []

-- | Look up a name; 'Nothing' if unbound in every frame.
envLookup :: String -> Env -> Maybe Value
envLookup s (Env kvs) = lookup s kvs

-- | Prepend a binding, shadowing any existing binding of the same
--   name in an outer frame.  Used by @defun@ and as the building
--   block of 'envPrepend'.
envCons :: String -> Value -> Env -> Env
envCons k v (Env kvs) = Env ((k, v) : kvs)

-- | Prepend a list of bindings at once (function parameters,
--   @let@ bindings).  The order inside the list is preserved; later
--   entries in the list are /deeper/ in the environment than earlier
--   ones, but for @let@ and parameter lists this doesn't matter
--   because names must be distinct.
envPrepend :: [(String, Value)] -> Env -> Env
envPrepend kvs (Env kvs') = Env (kvs ++ kvs')

-- | Drop @n@ bindings from the front.  Called after a @let@ or
--   closure body finishes, to restore the caller's frame.  The
--   argument is always the number of bindings just prepended with
--   'envPrepend', so this exactly undoes that prepend.
envDrop :: Int -> Env -> Env
envDrop n (Env kvs) = Env (drop n kvs)

-- | Replace the value of an /existing/ binding.  Returns 'Nothing' if
--   the name is not bound anywhere.
--
--   This is the reason top-level @(setq x 1)@ is an error: there is no
--   @x@ to replace.  Introducing a top-level name requires @defun@.
--   To allow @setq@ to insert, this function would need to fall through
--   to 'envCons' when the walk reaches 'Env' @[]@.  That change is
--   deliberately /not/ made — see the 'MiniLisp.Eval' module header.
envSet :: String -> Value -> Env -> Maybe Env
envSet _ _ (Env []) = Nothing
envSet n v (Env ((n', v') : rest))
  | n == n'   = Just (Env ((n, v) : rest))
  | otherwise = case envSet n v (Env rest) of
      Nothing          -> Nothing
      Just (Env rest') -> Just (Env ((n', v') : rest'))

-- | @cons@ at the value level: prepend to a proper list, or form a
--   dotted pair when the tail is not a list.  This is why
--   @(cons 1 '(2 3))@ gives @(1 2 3)@ but @(cons 1 2)@ gives @(1 . 2)@.
consValue :: Value -> Value -> Value
consValue a (VList xs) = VList (a : xs)
consValue a b          = VPair a b

-- | Runtime printer.  Strings are shown with quotes (via 'show' on
--   'String'); symbols are shown bare.  The output for every
--   constructor except 'VClosure' and 'VPrim' is valid MiniLisp source.
instance Show Value where
  show (VNum n)          = show n
  show (VFloat d)        = show d
  show (VStr s)          = show s
  show (VSym s)          = s
  show (VList [])        = "nil"
  show (VList xs)        = "(" ++ unwords (map show xs) ++ ")"
  show (VPair a b)       = showPair a b
  show (VPrim s)         = "#<prim " ++ s ++ ">"
  show (VClosure ps _ _) = "#<lambda (" ++ unwords ps ++ ")>"

-- | Render a dotted pair's spine, stopping with @. tail@ when the
--   tail is not itself a pair or list.
showPair :: Value -> Value -> String
showPair a b = "(" ++ chain (show a) b ++ ")"
  where
    chain acc (VPair x y) = acc ++ " " ++ chain (show x) y
    chain acc (VList ys)  = acc ++ concatMap (\v -> " " ++ show v) ys
    chain acc y           = acc ++ " . " ++ show y