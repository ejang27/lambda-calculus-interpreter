{- Author: <your name here>
   File: Lexer.hs

   Lexes the syntax for the Preλ interpreter

   2)
   a. if, <, 2, 1, then. true, else, false
   b. hello, people
   c. +, 12,h
   d. iff, thenn
   e. tok, 33, <=
-}

module Lexer where

import Data.Char
import Text.Read

import Token
import Syntax

-- Lex a Preλ expression into a list of tokens
-- Calls `error` if there is a lexical error (something that
-- doesn't lex)
lexPreL :: String -> [Token]
lexPreL [] = []
lexPreL str = lexNoPrefix (isIgnored str)

--lex1 : takes in a string and lexes it in to a token and returns the token and the rest of the string
lex1 :: String -> (Token, String)
lex1 [] = error "unimplemented"
lex1 str
 |identifierToken /= "" && not(identifierToken `elem` notIdentStrings) = (VarT identifierToken, iRestOfString)
 |keywordToken /= "" = (printKeyword keywordToken, kRestOfString)
 |boolToken /= "" = (LiteralT(BoolV (stringToBool boolToken)), bRestOfString)
 |numToken /= "" = (LiteralT(IntegerV (read numToken :: Integer)), nRestOfString)
 |operatorToken /= "" = (OpT (printOp operatorToken), oRestOfString)
 |otherwise = lex1 []
  where
    (keywordToken, kRestOfString) = isKeyword str ""
    (identifierToken, iRestOfString) = isIdentifier str
    (operatorToken, oRestOfString) = isOperator str ""
    (boolToken, bRestOfString) = isBool str ""
    (numToken, nRestOfString) = isNum str
    notIdentStrings = ["true", "false", "if", "then", "else", "not"]

--printKeyword : takes in keyword and returns the token version of it
printKeyword :: String -> Token
printKeyword str
 |str == "if" = IfT
 |str == "then" = ThenT
 |str == "else" = ElseT
 |str == "not" = NotT
 |str == "\\" = LambdaT
 |str == "." = DotT
 |str == "@" = AppT

--printKeyword : takes in operator and returns the token version of it
printOp :: String -> Op
printOp str
 |str == "+" = Plus               -- +
 |str == "-" = Minus              -- -
 |str == "*" = Times              -- *
 |str == "/" = Divides            -- /
 |str == "<" = LessThan           -- <
 |str == "<=" = LessThanEquals     -- <=
 |str == ">" = GreaterThan        -- >
 |str == ">=" = GreaterThanEquals  -- >=
 |str == "=" = Equals             -- =
 |str == "/=" = NotEquals

--isKeyword : takes in a string and returns string if it is a keyword
isKeyword :: String -> String -> (String, String)
isKeyword [] restStr = ([],restStr)
isKeyword str emptyStr
  |str `elem` keywordStrings = (str , emptyStr)
  |otherwise = isKeyword (init str) ([last str] ++ emptyStr)
    where
      keywordStrings = ["if", "then", "else", "not", "\\", ".", "@"]

--isIdentifier: takes in a string and returns string if it is an identifier
isIdentifier :: String -> (String, String)
isIdentifier [] = ([],[])
isIdentifier (x:xs)
 |isAlpha x = ([x] ++ first, second)
 |otherwise = ("", [x] ++ xs)
  where
  (first, second) = isIdentifier xs

--isNum : takes in a string and returns string if it is a digit
isNum :: String -> (String, String)
isNum [] = ("",[])
isNum (x:xs)
 |isDigit x = ([x]++first, second)
 |otherwise = ("", x:xs)
  where
   (first, second) = isNum xs

--stringToBool : takes in a string of boolean and returns the actual boolean value
stringToBool :: String -> Bool
stringToBool "true" = True
stringToBool "false" = False

--isBool: takes in a string and returns string if it is an boolean value
isBool :: String -> String -> (String, String)
isBool [] restStr = ([], restStr)
isBool str emptyStr
 |str `elem` boolStrings = (str , emptyStr)
 |otherwise = isBool (init str) ([last str] ++ emptyStr)
  where
    boolStrings = ["true", "false"]

--isOperator: takes in a string and returns string if it is an operator
isOperator :: String -> String -> (String, String)
isOperator [] restStr = ([],restStr)
isOperator str emptyStr
 |str `elem` operatorsStrings = (str , emptyStr)
 |otherwise = isOperator (init str) ([last str] ++ emptyStr)
   where
     operatorsStrings = ["+", "-", "*", "/", "<", "<=", ">", ">=", "=", "/="]

--isIgnored: takes in a string and returns the rest of the string that is not a comment or space or ()
isIgnored :: String -> String
isIgnored [] = []
isIgnored (x:xs)
 |x == '(' = isIgnored xs
 |x == ')' = isIgnored xs
 |isSpace x = isIgnored xs
 |length (x:xs) >= 2 && x == '{' = isIgnored (isComment xs)
 |otherwise = (x:xs)

--isComment : takes in a string and returns the rest of the string that is not a comment
isComment :: String -> String
isComment [] = []
isComment (x:xs)
 |x == '}'  = xs
 |otherwise = isComment (xs)

lexNoPrefix :: String -> [Token]
lexNoPrefix [] = []
lexNoPrefix str = [token] ++ lexPreL restOfString
  where
   (token, restOfString)=(lex1 str)
