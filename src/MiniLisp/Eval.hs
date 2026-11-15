{- |
Evaluator for MiniLisp.

The evaluator is a pure function

> eval :: Env -> Value -> Maybe (Env, Value)

Every step returns the (possibly updated) environment alongside the
result.  This threading lets @setq@ and @let@ propagate bindings
without any global mutable state.

Semantics worth knowing:

  * @nil@ is 'VList' @[]@; the only falsy value.
  * @t@ self-evaluates and cannot be shadowed by @let@ or @lambda@.
  * @setq@ /replaces/ an existing binding; it never introduces a new
    name.  A top-level @(setq x 1)@ therefore fails, because there is
    no @x@ anywhere in the environment.  Use @let@ or @defun@ to
    introduce a name.
  * @setq@ inside a closure body affects the closure's local frame
    only.  Closures capture their environment by value (an immutable
    'Env'); there are no mutable cells.  This means closures /cannot/
    see each other's mutations, and a mutation performed by one call is
    invisible to the next.  The alternative — diffing the environment
    before and after the body and copying changes back to the caller —
    was tried and removed because it silently gave the wrong answer on
    the second call of a counter closure.  If cross-frame mutation is
    needed, the fix is shared mutable state, not a hack in 'apply'.
-}
module MiniLisp.Eval
  ( eval, evalTop, evalProgram
  , apply, applyAll, evalArgs
  , primNames
  , isNil, isSym, bool
  , eq, eql, equal
  ) where

import MiniLisp.Types
  ( Value(..), Env(..)
  , envLookup, envCons, envPrepend, envDrop, envSet
  , consValue
  )

-- * Value predicates and comparisons

isNil :: Value -> Bool
isNil (VList []) = True
isNil _          = False

isSym :: Value -> Bool
isSym (VSym _) = True
isSym _        = False

isList :: Value -> Bool
isList (VList _)   = True
isList (VPair _ _) = True
isList _           = False

isCons :: Value -> Bool
isCons (VList (_:_)) = True
isCons (VPair _ _)   = True
isCons _             = False

-- | Convert a Haskell 'Bool' to a Lisp boolean: @t@ for 'True', @nil@
--   for 'False'.
bool :: Bool -> Value
bool True  = VSym "t"
bool False = VList []

-- | @eq@: identity for symbols and primitives; 'True' only for nil.
--   Numbers, floats, strings, lists, and pairs are never @eq@ to a
--   /like/ value — those comparisons belong to 'eql' or 'equal'.
eq :: Value -> Value -> Bool
eq (VSym a)   (VSym b)   = a == b
eq (VPrim a)  (VPrim b)  = a == b
eq (VList []) (VList []) = True
eq _          _          = False

-- | @eql@: 'eq' extended to numbers.  Integer @1@ and float @1.0@ are
--   /not/ @eql@ — comparisons are within a numeric type.
eql :: Value -> Value -> Bool
eql (VNum a)   (VNum b)   = a == b
eql (VFloat a) (VFloat b) = a == b
eql a b                   = eq a b

-- | @equal@: structural equality for strings, lists, and pairs.
--   Recursively compares both sides; falls back to 'eql' for atoms.
equal :: Value -> Value -> Bool
equal (VStr a)   (VStr b)   = a == b
equal (VList as) (VList bs) = length as == length bs && and (zipWith equal as bs)
equal (VPair a1 b1) (VPair a2 b2) = equal a1 a2 && equal b1 b2
equal a b = eql a b

-- * Primitives

-- | Names of all built-in primitives.  Any of these names, when
--   looked up as a variable and not shadowed, produces a 'VPrim'.
primNames :: [String]
primNames =
  [ "+", "-", "*", "/", "div", "mod", "abs"
  , "<", ">", "<=", ">=", "=", "/="
  , "eq", "eql", "equal"
  , "cons", "car", "cdr"
  , "null", "list", "listp", "consp", "atom", "numberp", "symbolp"
  ]

-- * Numeric helpers

-- | Apply a pair of numeric operations to two values: @intOp@ when both
--   are integers, @floatOp@ when either is a float.  Integer arguments
--   to @floatOp@ are promoted via @fromIntegral@.  Returns 'Nothing'
--   for any non-numeric combination, which is how the numeric
--   primitives report a type error.
withNumbers2
  :: (Integer -> Integer -> Value)   -- ^ int × int
  -> (Double  -> Double  -> Value)   -- ^ any float involved
  -> Env -> Value -> Value -> Maybe (Env, Value)
withNumbers2 intOp floatOp env a b = case (a, b) of
  (VNum x,   VNum y)   -> Just (env, intOp x y)
  (VNum x,   VFloat y) -> Just (env, floatOp (fromIntegral x) y)
  (VFloat x, VNum y)   -> Just (env, floatOp x (fromIntegral y))
  (VFloat x, VFloat y) -> Just (env, floatOp x y)
  _                    -> Nothing

-- | Two-argument arithmetic.  Int/int → int; anything float → float.
liftNum2 :: (Integer -> Integer -> Integer)
         -> (Double  -> Double  -> Double)
         -> Env -> Value -> Value -> Maybe (Env, Value)
liftNum2 iop fop = withNumbers2
  (\x y -> VNum (iop x y))
  (\x y -> VFloat (fop x y))

-- | Two-argument numeric comparison, returning a Lisp boolean.
liftCmp2 :: (Integer -> Integer -> Bool)
         -> (Double  -> Double  -> Bool)
         -> Env -> Value -> Value -> Maybe (Env, Value)
liftCmp2 iop fop = withNumbers2
  (\x y -> bool (iop x y))
  (\x y -> bool (fop x y))

-- | Two-argument float arithmetic — both arguments are promoted.
--   Used for @/@ so that even int/int yields a float.
liftFloat2 :: (Double -> Double -> Double)
           -> Env -> Value -> Value -> Maybe (Env, Value)
liftFloat2 f = withNumbers2
  (\x y -> VFloat (f (fromIntegral x) (fromIntegral y)))
  (\x y -> VFloat (f x y))

-- * Main evaluator

-- | Evaluate an expression in an environment, returning the (possibly
--   updated) environment and the result.  'Nothing' means an
--   evaluation error: type mismatch, arity error, unbound variable,
--   applying a non-function, division by zero for @div@/@mod@, etc.
--
--   Self-evaluating values are returned unchanged.  'VSym' performs a
--   lookup, falling back to 'VPrim' for primitive names, and finally
--   to failure.  'VList' with a known head is a special form or a
--   function call; 'VPair' is not a valid expression (it only exists
--   as the tail of a dotted list, and only 'quote' or a primitive can
--   consume it).
eval :: Env -> Value -> Maybe (Env, Value)
eval env v = case v of
  -- Self-evaluating values
  VNum _         -> Just (env, v)
  VFloat _       -> Just (env, v)
  VStr _         -> Just (env, v)
  VPrim _        -> Just (env, v)
  VClosure _ _ _ -> Just (env, v)
  VList []       -> Just (env, v)
  VPair _ _      -> Nothing

  -- Variable reference: @t@ self-evaluates; other symbols are looked
  -- up, then treated as primitive names, then fail.
  VSym "t" -> Just (env, v)
  VSym s   -> case envLookup s env of
    Just x  -> Just (env, x)
    Nothing
      | s `elem` primNames -> Just (env, VPrim s)
      | otherwise          -> Nothing

  -- Special forms
  VList (VSym "quote"  : [x])             -> Just (env, x)
  VList (VSym "cond"   : clauses)         -> evalCond env clauses
  VList (VSym "lambda" : VList ps : body) -> mkClosure env ps body
  VList (VSym "let"    : VList bs : body) -> evalLet env bs body
  VList (VSym "setq"   : args)            -> evalSetq env args
  VList (VSym "apply"  : f : [argExpr])   -> evalApply env f argExpr
  VList (VSym "defun"  : _)               -> Nothing   -- only at top level

  -- Function call
  VList (f : args) -> applyAll env f args

-- * Special forms

-- | @cond@: evaluate clause tests in order, run the body of the first
--   non-nil clause.  An empty body returns the test value itself (the
--   Common Lisp idiom @(cond (x))@ to test-and-yield).  No matching
--   clause yields @nil@.
evalCond :: Env -> [Value] -> Maybe (Env, Value)
evalCond env [] = Just (env, VList [])
evalCond env (VList (test : body) : clauses) =
  case eval env test of
    Nothing -> Nothing
    Just (env1, vt)
      | isNil vt  -> evalCond env1 clauses
      | null body -> Just (env1, vt)
      | otherwise -> evalSeq env1 body
evalCond _ _ = Nothing

-- | Evaluate a sequence of expressions, threading the environment, and
--   return the last value.  This is the primitive used by @cond@,
--   @let@, @lambda@, and @defun@ bodies — MiniLisp has no separate
--   @progn@.
evalSeq :: Env -> [Value] -> Maybe (Env, Value)
evalSeq env []     = Just (env, VList [])
evalSeq env [x]    = eval env x
evalSeq env (x:xs) = case eval env x of
  Nothing        -> Nothing
  Just (env1, _) -> evalSeq env1 xs

-- | @let@: bindings are evaluated in the /outer/ environment (parallel
--   semantics), then the body runs with all bindings in scope.  The
--   prepended bindings are dropped from the resulting environment
--   before returning, so mutations to @let@-locals don't leak out.
evalLet :: Env -> [Value] -> [Value] -> Maybe (Env, Value)
evalLet _ _ [] = Nothing
evalLet env bindings body =
  case evalBindings env bindings [] of
    Nothing -> Nothing
    Just newBindings ->
      case evalSeq (envPrepend newBindings env) body of
        Nothing           -> Nothing
        Just (envBody, v) -> Just (envDrop (length newBindings) envBody, v)

-- | Evaluate @let@ bindings.  Each must be @(name expr)@; names must be
--   distinct within the same @let@ (this catches typos and enforces
--   well-formedness of parameter lists too).
evalBindings :: Env -> [Value] -> [(String, Value)] -> Maybe [(String, Value)]
evalBindings _ [] acc = Just (reverse acc)
evalBindings env (VList [VSym n, e] : rest) acc
  | n `elem` map fst acc = Nothing
  | otherwise = case eval env e of
      Nothing     -> Nothing
      Just (_, v) -> evalBindings env rest ((n, v) : acc)
evalBindings _ _ _ = Nothing

-- | @setq@: assign to existing bindings, left-to-right.  Each pair is
--   evaluated in the environment produced by the previous pair, so
--   @(setq x 1 y x)@ binds @y@ to @1@ (the new value of @x@).
--   Fails if any name is unbound (see 'envSet').
evalSetq :: Env -> [Value] -> Maybe (Env, Value)
evalSetq env [] = Just (env, VList [])
evalSetq env [VSym n, e] =
  case eval env e of
    Nothing        -> Nothing
    Just (env1, v) -> case envSet n v env1 of
      Nothing   -> Nothing
      Just env2 -> Just (env2, v)
evalSetq env (VSym n : e : rest) =
  case eval env e of
    Nothing        -> Nothing
    Just (env1, v) -> case envSet n v env1 of
      Nothing   -> Nothing
      Just env2 -> evalSetq env2 rest
evalSetq _ _ = Nothing

-- | @(apply f argList)@: evaluate @f@, evaluate @argList@, require it
--   to be a proper list, and apply.  Non-list @argList@ is an error,
--   not a silent nil.
evalApply :: Env -> Value -> Value -> Maybe (Env, Value)
evalApply env f argExpr = case eval env f of
  Nothing         -> Nothing
  Just (env1, fn) -> case eval env1 argExpr of
    Nothing               -> Nothing
    Just (env2, VList as) -> apply env2 fn as
    Just _                -> Nothing

-- | Build a closure from a parameter list and body.  Rejects empty
--   bodies and duplicate parameter names.
mkClosure :: Env -> [Value] -> [Value] -> Maybe (Env, Value)
mkClosure env params body = case paramNames params of
  Just names | not (null body) -> Just (env, VClosure names body env)
  _                            -> Nothing

-- | Extract parameter names, rejecting duplicates and non-symbols.
paramNames :: [Value] -> Maybe [String]
paramNames ps =
  let ns = [n | VSym n <- ps]
  in if length ns == length ps && noDups ns then Just ns else Nothing

noDups :: Eq a => [a] -> Bool
noDups []       = True
noDups (x : xs) = x `notElem` xs && noDups xs

-- * Function application

-- | Evaluate @f@, then evaluate @args@ left-to-right, then 'apply'.
applyAll :: Env -> Value -> [Value] -> Maybe (Env, Value)
applyAll env f args = case eval env f of
  Nothing         -> Nothing
  Just (env1, fn) -> case evalArgs env1 args of
    Nothing         -> Nothing
    Just (env2, vs) -> apply env2 fn vs

-- | Evaluate a list of argument expressions left-to-right, threading
--   the environment so any @setq@ in an argument is visible to later
--   arguments.
evalArgs :: Env -> [Value] -> Maybe (Env, [Value])
evalArgs env []     = Just (env, [])
evalArgs env (a:as) = case eval env a of
  Nothing         -> Nothing
  Just (env1, v)  -> case evalArgs env1 as of
    Nothing         -> Nothing
    Just (env2, vs) -> Just (env2, v : vs)

-- | Apply a function value to already-evaluated arguments.
--
--   Primitives are dispatched by name and arity pattern — a missing
--   case means "arity or type error" and the call falls through to the
--   final 'Nothing'.  The numeric primitives delegate to 'liftNum2',
--   'liftCmp2', 'liftFloat2'; the rest match directly.
--
--   Closures extend their /captured/ environment with the parameters,
--   run the body, and return the /caller's/ environment unchanged.
--   That last part is the "setq is local" rule documented in the module
--   header: mutations the body performs die with the closure's frame,
--   and the caller sees no difference.
apply :: Env -> Value -> [Value] -> Maybe (Env, Value)
apply env (VPrim "+")   [a, b] = liftNum2 (+)  (+)  env a b
apply env (VPrim "-")   [a, b] = liftNum2 (-)  (-)  env a b
apply env (VPrim "*")   [a, b] = liftNum2 (*)  (*)  env a b
apply env (VPrim "/")   [a, b] = liftFloat2 (/) env a b
apply env (VPrim "div") [VNum a, VNum b] | b /= 0 = Just (env, VNum (a `div` b))
apply env (VPrim "mod") [VNum a, VNum b] | b /= 0 = Just (env, VNum (a `mod` b))
apply env (VPrim "-")   [VNum a]   = Just (env, VNum (negate a))
apply env (VPrim "-")   [VFloat a] = Just (env, VFloat (negate a))
apply env (VPrim "abs") [VNum a]   = Just (env, VNum (abs a))
apply env (VPrim "abs") [VFloat a] = Just (env, VFloat (abs a))
apply env (VPrim "<")   [a, b] = liftCmp2 (<)  (<)  env a b
apply env (VPrim ">")   [a, b] = liftCmp2 (>)  (>)  env a b
apply env (VPrim "<=")  [a, b] = liftCmp2 (<=) (<=) env a b
apply env (VPrim ">=")  [a, b] = liftCmp2 (>=) (>=) env a b
apply env (VPrim "=")   [a, b] = liftCmp2 (==) (==) env a b
apply env (VPrim "/=")  [a, b] = liftCmp2 (/=) (/=) env a b
apply env (VPrim "eq")    [a, b] = Just (env, bool (eq a b))
apply env (VPrim "eql")   [a, b] = Just (env, bool (eql a b))
apply env (VPrim "equal") [a, b] = Just (env, bool (equal a b))
apply env (VPrim "cons") [a, b]        = Just (env, consValue a b)
apply env (VPrim "car")  [VList (x:_)] = Just (env, x)
apply env (VPrim "car")  [VList []]    = Just (env, VList [])
apply env (VPrim "car")  [VPair x _]   = Just (env, x)
apply env (VPrim "cdr")  [VList (_:xs)] = Just (env, VList xs)
apply env (VPrim "cdr")  [VList []]    = Just (env, VList [])
apply env (VPrim "cdr")  [VPair _ y]   = Just (env, y)
apply env (VPrim "null") [v] = Just (env, bool (isNil v))
apply env (VPrim "list") vs  = Just (env, VList vs)
apply env (VPrim "listp")   [v] = Just (env, bool (isList v))
apply env (VPrim "consp")   [VList (_:_)] = Just (env, VSym "t")
apply env (VPrim "consp")   [VPair _ _]   = Just (env, VSym "t")
apply env (VPrim "consp")   [_]           = Just (env, VList [])
apply env (VPrim "atom")    [v] = Just (env, bool (not (isCons v)))
apply env (VPrim "numberp") [VNum _]   = Just (env, VSym "t")
apply env (VPrim "numberp") [VFloat _] = Just (env, VSym "t")
apply env (VPrim "numberp") [_]        = Just (env, VList [])
apply env (VPrim "symbolp") [VSym _] = Just (env, VSym "t")
apply env (VPrim "symbolp") [_]      = Just (env, VList [])

-- User-defined closure: bind parameters into the captured environment,
-- evaluate the body, and return the caller's environment unchanged.
-- See the module header for why mutations don't propagate.
apply env (VClosure ps body closureEnv) args
  | length ps == length args =
      case evalSeq (envPrepend (zip ps args) closureEnv) body of
        Nothing     -> Nothing
        Just (_, v) -> Just (env, v)

apply _ _ _ = Nothing

-- * Top-level evaluation

-- | Top-level evaluation.  Only here does @defun@ have meaning: it
--   binds a closure into the environment that the closure itself
--   captures, giving recursion.
--
--   The trick is the recursive @let@: @recEnv@ is the environment
--   /after/ the binding is added, and the closure is created with
--   @recEnv@ as its captured environment.  Because Haskell's @let@ is
--   lazy, this produces a cyclic structure (the environment contains a
--   closure whose captured environment is that same environment) —
--   which is exactly what "the function's name is visible inside its
--   own body" means.  Without it, @(defun fact (n) ... (fact ...))@
--   would fail at the recursive call because @fact@ would be unbound.
evalTop :: Env -> Value -> Maybe (Env, Value)
evalTop env (VList (VSym "defun" : VSym name : VList params : body))
  | not (null body)
  , Just names <- paramNames params
  = let recEnv = envCons name (VClosure names body recEnv) env
    in Just (recEnv, VSym name)
  | otherwise = Nothing
evalTop env e = eval env e

-- | Evaluate a sequence of top-level forms.  Returns the environment
--   after the /last/ form (so a REPL caller can thread it forward) and
--   the last form's value.  Any failure aborts the whole sequence.
evalProgram :: Env -> [Value] -> Maybe (Env, Value)
evalProgram _   []     = Nothing
evalProgram env [e]    = evalTop env e
evalProgram env (e:es) = case evalTop env e of
  Nothing        -> Nothing
  Just (env', _) -> evalProgram env' es