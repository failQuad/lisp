{- |
Reader (parser) for MiniLisp.

Grammar sketch:

> program  ::= expr*
> expr     ::= atom | list | quoted | string
> quoted   ::= "'" expr                     -- sugar for (quote expr)
> list     ::= "(" expr* ("." expr)? ")"
> atom     ::= integer | float | symbol | "nil"
> string   ::= '"' <chars> '"'
>
> Whitespace and @;@-to-end-of-line comments are skipped wherever
> whitespace is permitted.

Every parser is written in position-passing style: it returns
@Just (value, rest)@ on success, @Nothing@ on failure.
-}
module MiniLisp.Parser
  ( parseProgram
  , parseExpr, parseList, parseAtom, parseStr
  , classify
  , skip, skipAll, isDelim
  , mspan, mbreak
  ) where

import Data.Char (isSpace)
import MiniLisp.Types

-- * Top level

-- | Parse zero or more expressions until end of input.  Whitespace and
--   comments are allowed before, between, and after expressions.
parseProgram :: String -> Maybe [Value]
parseProgram = go . skipAll
  where
    go [] = Just []
    go s  = case parseExpr s of
      Nothing     -> Nothing
      Just (v, r) -> case go (skipAll r) of
        Nothing -> Nothing
        Just vs -> Just (v : vs)

-- * Skipping whitespace and comments

-- | Skip whitespace only.
skip :: String -> String
skip = dropWhile isSpace

-- | Skip whitespace and @;@-to-end-of-line comments, repeatedly.
--
--   Must /loop/: after a comment's newline, more whitespace (and more
--   comments) may follow, so a single @skip@ followed by a single
--   comment-skip is not enough.  This is subtle — getting it wrong
--   silently breaks forms like @(1 ; one\\n 2)@, because the space
--   before @2@ is left for 'parseAtom' to choke on.
skipAll :: String -> String
skipAll = go
  where
    go [] = []
    go (c : cs)
      | isSpace c = go cs
      | c == ';'  = go (dropWhile (/= '\n') cs)
      | otherwise = c : cs

-- | Characters that terminate an atom / symbol.  Includes @;@, so
--   @foo;bar@ tokenizes as @foo@ followed by a comment, not as a
--   single symbol @foo;bar@.
isDelim :: Char -> Bool
isDelim c = isSpace c || c `elem` "()';\"" 

-- * Expression parsing

parseExpr :: String -> Maybe (Value, String)
parseExpr []     = Nothing
parseExpr (c:cs)
  | c == '('  = parseList (skipAll cs) []
  | c == ')'  = Nothing
  | c == '\'' = case parseExpr (skipAll cs) of
      Nothing     -> Nothing
      Just (v, r) -> Just (VList [VSym "quote", v], r)
  | c == '"'  = parseStr cs
  | otherwise = parseAtom (c:cs)

-- | Parse a parenthesized list, accumulating elements in reverse.
--
--   Handles three shapes:
--
--     * @()@               — empty list (@nil@)
--     * @(a b c ...)@      — proper list
--     * @(a b . c)@        — dotted pair; the @.@ must be followed by
--                            a delimiter, and the tail expression
--                            must be the last thing before @)@
--
--   The @.@ case is checked /before/ falling through to 'parseExpr',
--   so a symbol like @.foo@ (unusual but legal) still parses as a
--   symbol — only a bare @.@ followed by a delimiter is dotted-pair
--   syntax.
parseList :: String -> [Value] -> Maybe (Value, String)
parseList s acc = case skipAll s of
  []      -> Nothing
  (')':r) -> Just (VList (reverse acc), r)
  ('.' : c : rest)
    | isDelim c ->
        if null acc
          then Nothing
          else case parseExpr (skipAll (c : rest)) of
            Nothing     -> Nothing
            Just (v, r) -> case skipAll r of
              (')' : r') -> Just (buildDotted (reverse acc) v, r')
              _          -> Nothing
  s'      -> case parseExpr s' of
    Nothing     -> Nothing
    Just (v, r) -> parseList r (v : acc)

parseAtom :: String -> Maybe (Value, String)
parseAtom s =
  let (tok, rest) = mspan (not . isDelim) s
  in if null tok then Nothing else Just (classify tok, rest)

parseStr :: String -> Maybe (Value, String)
parseStr s = case mbreak (== '"') s of
  (body, '"' : rest) -> Just (VStr body, rest)
  _                  -> Nothing

-- * Value construction

-- | Turn an atom's text into a value.  Tries 'Integer', then 'Double',
--   then falls back to 'VSym'.  The special token @nil@ becomes the
--   empty list (@VList []@), which is the canonical nil.
classify :: String -> Value
classify "nil" = VList []
classify t     = case reads t :: [(Integer, String)] of
  [(n, "")] -> VNum n
  _         -> case reads t :: [(Double, String)] of
    [(d, "")] -> VFloat d
    _         -> VSym t

-- | Fold a reversed list of prefix values and a tail into a dotted
--   chain.  @buildDotted [1, 2] 3@ gives the structure shown as
--   @(1 2 . 3)@ — that is, @consValue 1 (consValue 2 3)@.
buildDotted :: [Value] -> Value -> Value
buildDotted []       ys = ys
buildDotted (x : xs) ys = consValue x (buildDotted xs ys)

-- * Low-level scanners

-- | Consume the longest prefix satisfying the predicate.
mspan :: (Char -> Bool) -> String -> (String, String)
mspan _ [] = ([], [])
mspan p (x:xs)
  | p x       = case mspan p xs of
      (ys, zs) -> (x : ys, zs)
  | otherwise = ([], x:xs)

-- | Consume up to (but not including) the first char satisfying the
--   predicate.  Equivalent to @mspan (not . p)@.
mbreak :: (Char -> Bool) -> String -> (String, String)
mbreak p xs = mspan (not . p) xs