{- Author: Richard Eisenberg
   File: Eval.hs

   Defines an evaluator for Preλ
-}

module Eval where

import Syntax

-- Evaluate an expression to a value. Calls `error` if
-- this is impossible.
eval :: Expr -> Value
eval (OpE op (ex1) (ex2)) = performOp op (valToInt(eval (ex1))) (valToInt(eval (ex2)))
eval (IfE (ex1) (ex2) (ex3))
 |valToBool(eval(ex1))==True = eval(ex2)
 |otherwise = eval(ex3)
eval (ValueE (v)) = v
eval (NotE (ex)) = BoolV(not(valToBool(eval(ex))))
eval (AppE (ex1) (ex2)) = eval(subst expr str (eval(ex2)))
 where
   (str, expr) = lambVToStrEx (eval(ex1))
eval _ = error "unimplemented"

--valToInt : takes in integer value and returns an integer
valToInt :: Value -> Integer
valToInt (IntegerV (i)) = i
valToInt _ = error "Not integer"

--valToInt : takes in boolean value and returns an boolean
valToBool :: Value -> Bool
valToBool (BoolV (b)) = b
valToBool _ = error "Not boolean"

--lambVToStrEx : takes in a lambda value and returns the string and expression
lambVToStrEx :: Value -> (String, Expr)
lambVToStrEx (LambdaV (str) (ex)) = (str, ex)


-- All binary operators take two Integer arguments. This
-- function performs the operation on the arguments, returning
-- a Value.
performOp :: Op -> Integer -> Integer -> Value
performOp Plus a b = IntegerV (a+b)
performOp Minus a b = IntegerV (a-b)
performOp Times a b = IntegerV (a*b)
performOp Divides a b = IntegerV ((fromInteger a)`div`b)
performOp LessThan a b = BoolV (a<b)
performOp LessThanEquals a b = BoolV (a<=b)
performOp GreaterThan a b = BoolV (a>b)
performOp GreaterThanEquals a b = BoolV (a>=b)
performOp Equals a b = BoolV (a==b)
performOp NotEquals a b = BoolV (a/=b)

-- Substitute a value into an expression
-- If you want (expr)[x := val], call (subst expr "x" val)
subst :: Expr -> String -> Value -> Expr
subst (OpE op (ex1) (ex2)) str val = OpE op (subst(ex1) str val) (subst(ex2) str val)
subst (IfE (ex1) (ex2) (ex3)) str val = IfE (subst(ex1) str val) (subst(ex2) str val) (subst(ex3) str val)
subst (NotE (ex1)) str val = NotE (subst (ex1) str val)
subst (AppE (ex1) (ex2)) str val = AppE (subst(ex1) str val) (subst(ex2) str val)
subst (ValueE (LambdaV (s) (ex))) str val
 |s/=str = ValueE (LambdaV (s) (subst (ex) str val))
 |otherwise = ValueE (LambdaV (s) (ex))
subst (ValueE (IntegerV (i))) str val = ValueE (IntegerV (i))
subst (ValueE (BoolV (b))) str val = ValueE (BoolV (b))
subst (VarE(v)) str val
  |v==str = (ValueE(val))
  |otherwise = (VarE(v))
