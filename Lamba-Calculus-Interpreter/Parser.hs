{- Author: <your name here>
   File: Parser.hs

   Parses the Preλ syntax

   3)
   a. +
     / \
   -    3
  / \
5    4

  b.   if
     / \   \
    >  then  else
  /  \   \      \
5     3   true   *
                / \
               9   3

  c. no parse exists

  d. @
    / \
  \    5
 / \
x   +
   / \
  x   3

-}

module Parser where

import Token
import Syntax

-- Parse an expression, returning the parsed expression
-- and a list of unconsumed tokens
-- Calls `error` if the list of tokens has no valid parse.
parse :: [Token] -> (Expr, [Token])
parse [] = error "unimplemented"

parse (LiteralT l: rest) = (ValueE l, rest)

parse (VarT v: rest) = (VarE v, rest)

parse ((DotT):rest) = parse rest

parse ((LambdaT):VarT v: rest1)
 |(arg1, rest2) <- parse rest1
 =(mkLa (LambdaT) v arg1, rest2)

parse ((NotT) : rest1)
 |(arg1, rest2) <- parse rest1
 = (mkNot (NotT) arg1, rest2)

parse ((AppT) : rest1)
 |(arg1, rest2) <- parse rest1
 ,(arg2, rest3) <- parse rest2
 = (mkAp (AppT) arg1 arg2, rest3)

parse (OpT op : rest1)
 |(arg1, rest2) <- parse rest1
 ,(arg2, rest3) <- parse rest2
 = (mkOp (OpT op) arg1 arg2, rest3)

parse ((IfT):rest1)
 |(arg1, rest2) <- parse rest1
 ,(arg2, rest3) <- parse rest2
 ,(arg3, rest4) <- parse rest3
 = (mkIf (IfT) arg1 arg2 arg3, rest4)

parse ((ThenT):rest) = parse rest
parse ((ElseT):rest) = parse rest

--mkAp: takes in a AppT and returns AppE
mkAp :: Token -> Expr -> Expr -> Expr
mkAp (AppT) arg1 arg2 = AppE arg1 arg2

--mkOp: takes in a OpT and returns OpE
mkOp :: Token -> Expr -> Expr -> Expr
mkOp (OpT Plus) arg1 arg2 = OpE Plus arg1 arg2
mkOp (OpT Minus) arg1 arg2 = OpE Minus arg1 arg2
mkOp (OpT Times) arg1 arg2 = OpE Times arg1 arg2
mkOp (OpT Divides) arg1 arg2 = OpE Divides arg1 arg2
mkOp (OpT LessThan) arg1 arg2 = OpE LessThan arg1 arg2
mkOp (OpT LessThanEquals) arg1 arg2 = OpE LessThanEquals arg1 arg2
mkOp (OpT GreaterThan) arg1 arg2 = OpE GreaterThan arg1 arg2
mkOp (OpT GreaterThanEquals) arg1 arg2 = OpE GreaterThanEquals arg1 arg2
mkOp (OpT Equals) arg1 arg2 = OpE Equals arg1 arg2
mkOp (OpT NotEquals) arg1 arg2 = OpE NotEquals arg1 arg2

--mkIf: takes in a IfT and returns IfE
mkIf :: Token -> Expr ->Expr -> Expr -> Expr
mkIf (IfT) arg1 arg2 arg3 = IfE arg1 arg2 arg3

--mkNot: takes in a NotT and returns NotE
mkNot :: Token -> Expr -> Expr
mkNot (NotT) arg1 = NotE arg1

--mkLa: takes in a LambdaT and returns ValueE LambdaV
mkLa :: Token -> String -> Expr -> Expr
mkLa (LambdaT) v arg1 = ValueE (LambdaV v arg1)
