{- Author: Richard Eisenberg
   File: Token.hs

   Defines lexical tokens.

   5)
   a. [IfT, OpT LessThan, LiteralT (IntegerV 2),LiteralT (IntegerV 1), ThenT, LiteralT (BoolV True), ElseT, LiteralT (BoolV False)]
   b. [VarT "hello", VarT "people"]
   c. [OpT Plus, LiteralT (IntegerV 12), VarT "h" ]
   d. [VarT "iff", VarT "thenn"]
   e. [VarT "tok", LiteralT (IntegerV 33), OpT LessThanEquals]
-}

module Token where

import Syntax

data Token
  = LiteralT Value     -- numbers, booleans
  | IfT                -- "if"
  | ThenT              -- "then"
  | ElseT              -- "else"
  | OpT Op             -- binary operators
  | NotT               -- "not"
  | LambdaT            -- "\"
  | VarT String        -- e.g. "x" "y"
  | DotT               -- "."
  | AppT               -- "@"
  deriving Show
