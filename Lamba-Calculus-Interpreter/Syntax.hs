{- Author: Richard Eisenberg
   File: Syntax.hs

   Basic definitions for the abstract syntax tree.

   4)
   a. OpE ( Plus OpE( Minus ValueE(IntegerV 5) ValueE(IntegerV 4) ) ValueE(IntegerV 3) )
   b. IfE ( OpE( LessThan ValueE(IntegerV 5) ValueE(IntegerV 3)) ValueE(BoolV true) OpE( Times ValueE(IntegerV 9) ValueE(IntegerV 3)))
   d. AppE (ValueE(LambdaV "x" (OpE(Plus VarE "x" ValueE(IntegerV 3))))) (ValueE (IntegerV 5))
-}

module Syntax where

-- Values cannot be evaluated further.
data Value
  = IntegerV Integer     -- Integers, like 5 and 674
  | BoolV Bool           -- Booleans, like true and false
  | LambdaV String Expr  -- Lambda expressions (which are values,
                         -- as you can't evaluate them further)
  deriving Show

-- Binary operators
data Op
  = Plus               -- +
  | Minus              -- -
  | Times              -- *
  | Divides            -- /
  | LessThan           -- <
  | LessThanEquals     -- <=
  | GreaterThan        -- >
  | GreaterThanEquals  -- >=
  | Equals             -- =
  | NotEquals          -- /=
  deriving Show

-- Expressions
data Expr
  = IfE Expr Expr Expr   -- if e1 then e2 else e3
  | OpE Op Expr Expr     -- e.g. + e1 e2
  | NotE Expr            -- not e1
  | ValueE Value         -- values are expressions, too
  | VarE String          -- e.g. x
  | AppE Expr Expr       -- @ e1 e2
                         -- (separate from other binary operators because
                         -- @ evaluates so differently)
  deriving Show
