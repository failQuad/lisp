{- |
Debug utility: print the AST produced by the parser for a program on
stdin, without evaluating it.  Intended for development, not for user
output.
-}
module Main(main) where

import MiniLisp.Types
import MiniLisp.Parser (parseProgram)

showAST :: [Value] -> String
showAST = unlines . concatMap (renderValue 0)

showASTFromSource :: String -> String
showASTFromSource src = case parseProgram src of
  Nothing -> "parse error"
  Just vs -> showAST vs

renderValue :: Int -> Value -> [String]
renderValue indent v = case v of
  VNum n          -> [pad indent ++ "VNum "      ++ show n]
  VFloat d        -> [pad indent ++ "VFloat "    ++ show d]
  VStr s          -> [pad indent ++ "VStr "      ++ show s]
  VSym s          -> [pad indent ++ "VSym "      ++ show s]
  VPrim s         -> [pad indent ++ "VPrim "     ++ show s]
  VClosure ps _ _ -> [pad indent ++ "VClosure (" ++ unwords ps ++ ")"]
  VList []        -> [pad indent ++ "VList []"]
  VList xs        ->
    [ pad indent ++ "VList [" ]
    ++ concatMap (renderValue (indent + 2)) xs
    ++ [ pad indent ++ "]" ]
  VPair a b       ->
    [ pad indent ++ "VPair" ]
    ++ renderValue (indent + 2) a
    ++ renderValue (indent + 2) b

pad :: Int -> String
pad n = replicate n ' '

main :: IO ()
main = getContents >>= putStr . showASTFromSource