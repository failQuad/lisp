module Main(main) where

import MiniLisp.Eval   (isNil, isSym, eq, eql, equal)
import MiniLisp.Parser (classify, parseExpr, parseProgram)
import MiniLisp.Repl   (balanced)
import MiniLisp.Run    (run)
import MiniLisp.Types

data Result = Result
  { rName   :: String
  , rPassed :: Bool
  , rDetail :: String
  }

testTrue :: String -> Bool -> Result
testTrue name ok = Result name ok (if ok then "" else "expected #t, got #f")

testEq :: (Show a) => String -> a -> a -> Result
testEq name expected actual =
  let e = show expected
      a = show actual
  in Result name (e == a)
       (if e == a then "" else "expected " ++ e ++ ", got " ++ a)

renderLine :: Result -> String
renderLine r
  | rPassed r = "PASS " ++ rName r
  | otherwise = "FAIL " ++ rName r ++ "\n  " ++ rDetail r

summarize :: [Result] -> String
summarize rs =
  let total  = length rs
      passed = length (filter rPassed rs)
  in unlines (map renderLine rs)
     ++ "\n"
     ++ show passed ++ "/" ++ show total ++ " tests passed\n"
     ++ (if passed == total then "ALL PASSED" else "FAILURES")

classifyTests :: [Result]
classifyTests =
  [ testEq "classify.nil"    (VList [])   (classify "nil")
  , testEq "classify.t"      (VSym "t")   (classify "t")
  , testEq "classify.num"    (VNum 42)    (classify "42")
  , testEq "classify.neg"    (VNum (-7))  (classify "-7")
  , testEq "classify.sym"    (VSym "foo") (classify "foo")
  , testEq "classify.notnum" (VSym "3x")  (classify "3x")
  ]

parseExprTests :: [Result]
parseExprTests =
  [ testEq "parse.num"   (Just (VNum 42, ""))    (parseExpr "42")
  , testEq "parse.trail" (Just (VNum 42, " 43")) (parseExpr "42 43")
  , testEq "parse.nil"   (Just (VList [], ""))   (parseExpr "()")
  , testEq "parse.list"  (Just (VList [VNum 1, VNum 2, VNum 3], ""))
                          (parseExpr "(1 2 3)")
  , testEq "parse.quote" (Just (VList [VSym "quote", VSym "x"], ""))
                          (parseExpr "'x")
  , testEq "parse.nested"
      (Just (VList [VSym "a", VList [VSym "b", VSym "c"], VSym "d"], ""))
      (parseExpr "(a (b c) d)")
  , testEq "parse.dotted" (Just (VPair (VNum 1) (VNum 2), ""))
                          (parseExpr "(1 . 2)")
  , testEq "parse.dotted-chain"
      (Just (VPair (VNum 1) (VPair (VNum 2) (VNum 3)), ""))
      (parseExpr "(1 2 . 3)")
  , testEq "parse.dotted-tail-list"
      (Just (VList [VNum 1, VNum 2], ""))
      (parseExpr "(1 . (2))")
  , testEq "parse.unbal" Nothing (parseExpr "(")
  , testEq "parse.stray" Nothing (parseExpr ")")
  , testEq "parse.str"          (Just (VStr "hello", "")) (parseExpr "\"hello\"")
  , testEq "parse.str-empty"    (Just (VStr "", ""))      (parseExpr "\"\"")
  , testEq "parse.str-unterm"   Nothing                   (parseExpr "\"oops")
  ]

parseProgramTests :: [Result]
parseProgramTests =
  [ testEq "prog.empty" (Just []) (parseProgram "")
  , testEq "prog.space" (Just []) (parseProgram "   ")
  , testEq "prog.atoms" (Just [VNum 1, VNum 2, VNum 3]) (parseProgram "1 2 3")
  , testEq "prog.exprs"
      (Just [ VList [VSym "+", VNum 1, VNum 2]
            , VList [VSym "*", VNum 3, VNum 4] ])
      (parseProgram "(+ 1 2) (* 3 4)")
  , testEq "prog.err" Nothing (parseProgram ")")
  ]

commentTests :: [Result]
commentTests =
  [ testEq "comment.trailing" (Just [VNum 42])
      (parseProgram "42 ; trailing")
  , testEq "comment.multi" (Just [VNum 1, VNum 2])
      (parseProgram "1 ; c1\n2 ; c2")
  , testEq "comment.in-list" (Just [VList [VNum 1, VNum 2]])
      (parseProgram "(1 ; one\n 2)")
  , testEq "comment.only" (Just [])
      (parseProgram "; nothing here")
  , testEq "comment.not-in-string" (Just [VStr "a ; b"])
      (parseProgram "\"a ; b\"")
  ]

predicateTests :: [Result]
predicateTests =
  [ testTrue "eq.sym-t"   (eq (VSym "a") (VSym "a"))
  , testTrue "eq.sym-f"   (not (eq (VSym "a") (VSym "b")))
  , testTrue "eq.nil-t"   (eq (VList []) (VList []))
  , testTrue "eq.num-f"   (not (eq (VNum 1) (VNum 1)))
  , testTrue "eq.float-f" (not (eq (VFloat 1.5) (VFloat 1.5)))
  , testTrue "eq.str-f"   (not (eq (VStr "a") (VStr "a")))
  , testTrue "eq.list-f"  (not (eq (VList [VNum 1]) (VList [VNum 1])))
  , testTrue "eq.pair-f"  (not (eq (VPair (VNum 1) (VNum 2)) (VPair (VNum 1) (VNum 2))))
  , testTrue "eq.mixed-f" (not (eq (VNum 1) (VSym "1")))
  , testTrue "eq.prim-t"  (eq (VPrim "car") (VPrim "car"))
  , testTrue "eq.closure"
      (not (eq (VClosure [] [] emptyEnv) (VClosure [] [] emptyEnv)))
  , testTrue "eql.num-t"     (eql (VNum 1) (VNum 1))
  , testTrue "eql.num-f"     (not (eql (VNum 1) (VNum 2)))
  , testTrue "eql.float-t"   (eql (VFloat 1.5) (VFloat 1.5))
  , testTrue "eql.float-f"   (not (eql (VFloat 1.5) (VFloat 2.5)))
  , testTrue "eql.int-float" (not (eql (VNum 1) (VFloat 1.0)))
  , testTrue "eql.sym-t"     (eql (VSym "a") (VSym "a"))
  , testTrue "eql.nil-t"     (eql (VList []) (VList []))
  , testTrue "eql.str-f"     (not (eql (VStr "a") (VStr "a")))
  , testTrue "eql.list-f"    (not (eql (VList [VNum 1]) (VList [VNum 1])))
  , testTrue "equal.num-t"    (equal (VNum 1) (VNum 1))
  , testTrue "equal.num-f"    (not (equal (VNum 1) (VNum 2)))
  , testTrue "equal.float-t"  (equal (VFloat 1.5) (VFloat 1.5))
  , testTrue "equal.float-f"  (not (equal (VFloat 1.5) (VFloat 2.5)))
  , testTrue "equal.sym-t"    (equal (VSym "a") (VSym "a"))
  , testTrue "equal.str-t"    (equal (VStr "a") (VStr "a"))
  , testTrue "equal.str-f"    (not (equal (VStr "a") (VStr "b")))
  , testTrue "equal.str-sym"  (not (equal (VStr "a") (VSym "a")))
  , testTrue "equal.list-t"   (equal (VList [VNum 1]) (VList [VNum 1]))
  , testTrue "equal.list-f"   (not (equal (VList [VNum 1]) (VList [VNum 2])))
  , testTrue "equal.nested"
      (equal (VList [VList [VNum 1, VNum 2]]) (VList [VList [VNum 1, VNum 2]]))
  , testTrue "equal.pair-t"    (equal (VPair (VNum 1) (VNum 2)) (VPair (VNum 1) (VNum 2)))
  , testTrue "equal.pair-f"    (not (equal (VPair (VNum 1) (VNum 2)) (VPair (VNum 1) (VNum 3))))
  , testTrue "equal.mixed-f"   (not (equal (VNum 1) (VSym "1")))
  , testTrue "isNil.nil"  (isNil (VList []))
  , testTrue "isNil.num"  (not (isNil (VNum 0)))
  , testTrue "isNil.str"  (not (isNil (VStr "")))
  , testTrue "isNil.list" (not (isNil (VList [VNum 1])))
  , testTrue "isNil.pair" (not (isNil (VPair (VNum 1) (VNum 2))))
  , testTrue "isSym.sym"  (isSym (VSym "x"))
  , testTrue "isSym.num"  (not (isSym (VNum 0)))
  , testTrue "isSym.str"  (not (isSym (VStr "x")))
  , testTrue "isSym.list" (not (isSym (VList [])))
  ]

balancedTests :: [Result]
balancedTests =
  [ testTrue "balanced.empty"        (balanced "")
  , testTrue "balanced.atom"         (balanced "foo")
  , testTrue "balanced.pair"         (balanced "()")
  , testTrue "balanced.nested"       (balanced "(a (b c) d)")
  , testTrue "balanced.string-open"  (balanced "\"(\"")
  , testTrue "balanced.string-close" (balanced "\")\"")
  , testTrue "balanced.unterminated-string" (balanced "\"(")
  , testTrue "balanced.open"         (not (balanced "("))
  , testTrue "balanced.two-open"     (not (balanced "((a)"))
  , testTrue "balanced.close"        (not (balanced ")"))
  , testTrue "balanced.close-open"   (not (balanced ")("))
  , testTrue "balanced.extra-close"  (not (balanced "())"))
  ]

runTests :: [Result]
runTests =
  [ testEq "run.num" "42"  (run "42")
  , testEq "run.nil" "nil" (run "nil")
  , testEq "run.t"   "t"   (run "t")
  , testEq "run.quote"      "foo"     (run "'foo")
  , testEq "run.quote-list" "(1 2 3)" (run "'(1 2 3)")
  , testEq "run.quote-dotted" "(1 . 2)" (run "'(1 . 2)")
  , testEq "run.add" "3"  (run "(+ 1 2)")
  , testEq "run.sub" "2"  (run "(- 5 3)")
  , testEq "run.neg" "-5" (run "(- 5)")
  , testEq "run.mul" "42" (run "(* 6 7)")
  , testEq "run.div"     "3"  (run "(div 10 3)")
  , testEq "run.div-neg" "-4" (run "(div -7 2)")
  , testEq "run.mod"     "1"  (run "(mod 10 3)")
  , testEq "run.slash-int"        "3.3333333333333335" (run "(/ 10 3)")
  , testEq "run.slash-float"      "0.5"                (run "(/ 1.0 2.0)")
  , testEq "run.slash-mixed"      "0.5"                (run "(/ 1 2.0)")
  , testEq "run.slash-by-zero"    "Infinity"           (run "(/ 1.0 0.0)")
  , testEq "run.div-by-zero"      "eval error"         (run "(div 1 0)")
  , testEq "run.div-float-arg"    "eval error"         (run "(div 10.0 3)")
  , testEq "run.div-mixed-arg"    "eval error"         (run "(div 10 3.0)")
  , testEq "run.abs" "5"  (run "(abs -5)")
  , testEq "run.float-lit"        "3.14"  (run "3.14")
  , testEq "run.float-neg"        "-1.5"  (run "-1.5")
  , testEq "run.float-exp"        "1000.0" (run "1e3")
  , testEq "run.float-add"        "4.0"   (run "(+ 1.5 2.5)")
  , testEq "run.float-mixed-add"  "3.5"   (run "(+ 1 2.5)")
  , testEq "run.float-mixed-sub"  "0.5"   (run "(- 1.0 0.5)")
  , testEq "run.float-mul"        "6.0"   (run "(* 2.0 3)")
  , testEq "run.float-unary-neg"  "-2.5"  (run "(- 2.5)")
  , testEq "run.float-abs"        "2.5"   (run "(abs -2.5)")
  , testEq "run.float-lt"         "t"     (run "(< 1.5 2.0)")
  , testEq "run.float-eq-mixed"   "t"     (run "(= 1 1.0)")
  , testEq "run.float-eq-f"       "nil"   (run "(= 1.5 2.5)")
  , testEq "run.float-numberp"    "t"     (run "(numberp 1.5)")
  , testEq "run.float-eq"         "nil"   (run "(eq 1.5 1.5)")
  , testEq "run.float-eql"        "t"     (run "(eql 1.5 1.5)")
  , testEq "run.float-eql-mixed"  "nil"   (run "(eql 1 1.0)")
  , testEq "run.float-equal"      "t"     (run "(equal 1.5 1.5)")
  , testEq "run.float-list"       "(1.5 2.5)" (run "(list 1.5 2.5)")
  , testEq "run.float-car"        "1.5"   (run "(car (list 1.5 2.5))")
  , testEq "run.lt-t" "t"   (run "(< 1 2)")
  , testEq "run.lt-f" "nil" (run "(< 2 1)")
  , testEq "run.eq-num-t" "t"   (run "(= 5 5)")
  , testEq "run.eq-num-f" "nil" (run "(= 5 6)")
  , testEq "run.gt-t" "t"   (run "(> 2 1)")
  , testEq "run.gt-f" "nil" (run "(> 1 2)")
  , testEq "run.le-t" "t"   (run "(<= 1 1)")
  , testEq "run.le-f" "nil" (run "(<= 2 1)")
  , testEq "run.ge-t" "t"   (run "(>= 2 1)")
  , testEq "run.ge-f" "nil" (run "(>= 1 2)")
  , testEq "run.ne-t" "t"   (run "(/= 1 2)")
  , testEq "run.ne-f" "nil" (run "(/= 1 1)")
  , testEq "run.cond-empty"  "nil" (run "(cond)")
  , testEq "run.cond-first"  "yes" (run "(cond ((= 1 1) 'yes))")
  , testEq "run.cond-second" "2"   (run "(cond ((= 1 2) 1) (t 2))")
  , testEq "run.cond-multi"  "3"   (run "(cond (t 1 2 3))")
  , testEq "run.cond-t-empty" "t"  (run "(cond (t))")
  , testEq "run.cond-num-truthy" "yes" (run "(cond (0 'yes))")
  , testEq "run.cond-str-truthy" "yes" (run "(cond (\"hi\" 'yes))")
  , testEq "run.let"         "3" (run "(let ((x 3)) x)")
  , testEq "run.let-sum"     "7" (run "(let ((x 3) (y 4)) (+ x y))")
  , testEq "run.let-shadow"  "2" (run "(let ((x 1)) (let ((x 2)) x))")
  , testEq "run.let-closed"  "4" (run "(let ((x 1)) (let ((x (+ x 3))) x))")
  , testEq "run.lambda-id"      "5"  (run "((lambda (x) x) 5)")
  , testEq "run.lambda-sum"     "7"  (run "((lambda (x y) (+ x y)) 3 4)")
  , testEq "run.lambda-scope"   "15" (run "(let ((y 10)) ((lambda (x) (+ x y)) 5))")
  , testEq "run.lambda-closure" "11"
      (run "(let ((f (lambda (x) (+ x 1)))) (f 10))")
  , testEq "run.call-expr-head" "25"
      (run "((let ((fs (list (lambda (x) (* x x))))) (car fs)) 5)")
  , testEq "run.setq"       "2"  (run "(let ((x 1)) (setq x 2) x)")
  , testEq "run.setq-multi" "30"
      (run "(let ((x 1) (y 2)) (setq x 10 y 20) (+ x y))")
  , testEq "run.apply-prim" "1"  (run "(apply car '((1 2)))")
  , testEq "run.apply-lam"  "3"  (run "(apply (lambda (x y) (+ x y)) '(1 2))")
  , testEq "run.apply-nonlist" "eval error" (run "(apply car 5)")
  , testEq "run.car"     "1"       (run "(car '(1 2 3))")
  , testEq "run.cdr"     "(2 3)"   (run "(cdr '(1 2 3))")
  , testEq "run.cons"    "(1 2 3)" (run "(cons 1 '(2 3))")
  , testEq "run.cons-dotted" "(1 . 2)" (run "(cons 1 2)")
  , testEq "run.cons-chain"  "(1 2 . 3)" (run "(cons 1 (cons 2 3))")
  , testEq "run.pair-car"    "1"  (run "(car (cons 1 2))")
  , testEq "run.pair-cdr"    "2"  (run "(cdr (cons 1 2))")
  , testEq "run.consp-pair"  "t"  (run "(consp (cons 1 2))")
  , testEq "run.listp-pair"  "t"  (run "(listp (cons 1 2))")
  , testEq "run.atom-pair"   "nil" (run "(atom (cons 1 2))")
  , testEq "run.list"    "(1 2 3)" (run "(list 1 2 3)")
  , testEq "run.null-t"  "t"       (run "(null nil)")
  , testEq "run.null-f"  "nil"     (run "(null '(1))")
  , testEq "run.null-pair" "nil"   (run "(null (cons 1 2))")
  , testEq "run.listp"   "t"       (run "(listp '(1))")
  , testEq "run.consp"   "t"       (run "(consp '(1))")
  , testEq "run.atom-t"  "t"       (run "(atom 1)")
  , testEq "run.atom-f"  "nil"     (run "(atom '(1))")
  , testEq "run.numberp-t"     "t"   (run "(numberp 1)")
  , testEq "run.numberp-str"   "nil" (run "(numberp \"a\")")
  , testEq "run.numberp-sym"   "nil" (run "(numberp 'a)")
  , testEq "run.numberp-list"  "nil" (run "(numberp '(1))")
  , testEq "run.numberp-nil"   "nil" (run "(numberp nil)")
  , testEq "run.symbolp-quoted" "t"   (run "(symbolp 'a)")
  , testEq "run.symbolp-t"      "t"   (run "(symbolp t)")
  , testEq "run.symbolp-num"    "nil" (run "(symbolp 1)")
  , testEq "run.symbolp-str"    "nil" (run "(symbolp \"a\")")
  , testEq "run.symbolp-nil"    "nil" (run "(symbolp nil)")
  , testEq "run.symbolp-list"   "nil" (run "(symbolp '(1))")
  , testEq "run.symbolp-prim"   "nil" (run "(symbolp car)")
  , testEq "run.not-via-null-t" "t"   (run "(null (= 1 2))")
  , testEq "run.not-via-null-f" "nil" (run "(null (= 1 1))")
  , testEq "run.err-not"        "eval error" (run "(not t)")
  , testEq "run.defun-id"   "42"  (run "(defun id (x) x) (id 42)")
  , testEq "run.defun-fact" "120"
      (run "(defun fact (n) (cond ((< n 2) 1) (t (* n (fact (- n 1)))))) (fact 5)")
  , testEq "run.defun-fact-0"  "1"
      (run "(defun fact (n) (cond ((< n 2) 1) (t (* n (fact (- n 1)))))) (fact 0)")
  , testEq "run.defun-fact-10" "3628800"
      (run "(defun fact (n) (cond ((< n 2) 1) (t (* n (fact (- n 1)))))) (fact 10)")
  , testEq "run.defun-zero"    "42"
      (run "(defun f () 42) (f)")
  , testEq "run.defun-multi" "27"
      (run "(defun sq (x) (* x x)) (defun cube (x) (* x (sq x))) (cube 3)")
  , testEq "run.str-basic"  "\"hello\""     (run "\"hello\"")
  , testEq "run.str-empty"  "\"\""          (run "\"\"")
  , testEq "run.str-quotes" "\"a b\""       (run "\"a b\"")
  , testEq "run.str-list"   "(\"a\" \"b\")" (run "(list \"a\" \"b\")")
  , testEq "run.str-car"    "\"x\""         (run "(car (list \"x\"))")
  , testEq "run.eq-sym"        "t"   (run "(eq 'a 'a)")
  , testEq "run.eq-sym-f"      "nil" (run "(eq 'a 'b)")
  , testEq "run.eq-nil"        "t"   (run "(eq nil nil)")
  , testEq "run.eq-num"        "nil" (run "(eq 1 1)")
  , testEq "run.eql-num"       "t"   (run "(eql 1 1)")
  , testEq "run.eql-str"       "nil" (run "(eql \"a\" \"a\")")
  , testEq "run.eql-list"      "nil" (run "(eql '(1) '(1))")
  , testEq "run.equal-num"     "t"   (run "(equal 1 1)")
  , testEq "run.equal-str"     "t"   (run "(equal \"a\" \"a\")")
  , testEq "run.equal-str-f"   "nil" (run "(equal \"a\" \"b\")")
  , testEq "run.equal-list"    "t"   (run "(equal '(1 2) '(1 2))")
  , testEq "run.equal-list-f"  "nil" (run "(equal '(1 2) '(1 3))")
  , testEq "run.equal-nested"  "t"   (run "(equal '(1 (2 3)) '(1 (2 3)))")
  , testEq "run.equal-pair"    "t"   (run "(equal (cons 1 2) (cons 1 2))")
  , testEq "run.equal-str-sym" "nil" (run "(equal \"a\" 'a)")
  , testEq "run.lambda-dup" "eval error" (run "((lambda (x x) x) 1 2)")
  , testEq "run.defun-dup"  "eval error" (run "(defun f (x x) x) (f 1 2)")
  , testEq "run.let-dup"    "eval error" (run "(let ((x 1) (x 2)) x)")
  , testEq "run.lambda-zero" "42" (run "((lambda () 42))")
  , testEq "run.err-keyword" "eval error" (run ":foo")
  , testEq "run.err-funcall" "eval error" (run "(funcall car '(1 2))")
  , testEq "run.err-parse"        "parse error" (run "(")
  , testEq "run.err-eval-unbound" "eval error"  (run "(foo 1)")
  , testEq "run.err-eval-arity"   "eval error"  (run "(+ 1)")
  , testEq "run.err-eval-notfn"   "eval error"  (run "(1 2)")
  , testEq "run.err-setq-unbound" "eval error"  (run "(setq zzz 1)")
  , testEq "run.empty"      "" (run "")
  , testEq "run.whitespace" "" (run "   ")
  , testEq "run.cx-chain" "16"
      (run "(defun sq (x) (* x x)) (defun quad (x) (sq (sq x))) (quad 2)")
  , testEq "run.cx-nested-helpers" "20"
      (run "(defun sq (x) (* x x)) (defun sum-sq (a b) (+ (sq a) (sq b))) (defun hyp-sq (a b) (sum-sq a b)) (hyp-sq 2 4)")
  , testEq "run.cx-setq-triple" "20"
      (run "(let ((x 1) (y 2) (z 3)) (setq x (* x y)) (setq y (* y z)) (setq z (* x y)) (+ x (+ y z)))")
  , testEq "run.cx-setq-then-defun" "4"
      (run "(defun f1 () 1) (defun f2 () 2) (let ((x (f1))) (let ((y (f2))) (+ x (+ y (f1)))))")
  , testEq "run.cx-multi-setq-multi-defun" "337"
      (run "(defun sq (x) (* x x)) (defun sum-sq (a b) (+ (sq a) (sq b))) (let ((x 3) (y 4)) (setq x (sq x)) (setq y (sq y)) (sum-sq x y))")
  -- Closure setq semantics: mutations stay local to the closure's frame.
  , testEq "run.closure-setq-is-local" "1"
      (run "(let ((x 1)) ((lambda () (setq x 2))) x)")
  -- Comments should be ignored by the reader.
  , testEq "run.comment" "2" (run "1 ; comment\n2\n; trailing")
  ]

allTests :: [Result]
allTests = concat
  [ classifyTests
  , parseExprTests
  , parseProgramTests
  , commentTests
  , predicateTests
  , balancedTests
  , runTests
  ]

suite :: String
suite = summarize allTests

main :: IO ()
main = putStrLn suite