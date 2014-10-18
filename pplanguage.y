{
module Main where

import System.Environment
import Data.Char
}


%name go
%tokentype { Token }
%error { parseError }

%token
      while           { TokenWhile }
      if              { TokenIf }
      then            { TokenThen }
      else            { TokenElse }
      end             { TokenEnd }
      print           { TokenPrint }
      read            { TokenRead }
      int             { TokenInt $$ }
      string          { TokenString $$ }
      bool            { TokenBool $$ }
      var             { TokenVar $$ }
      '='             { TokenAt }
      '$'             { TokenEqual }
      '<'             { TokenLess }
      '>'             { TokenGreater }
      '+'             { TokenPlus }
      '-'             { TokenMinus }
      '*'             { TokenTimes }
      '/'             { TokenDiv }
      '('             { TokenOB }
      ')'             { TokenCB }
      '\n'            { TokenLC }

%nonassoc '>' '<'
%left '+' '-'
%left '*' '/'
%left '$'
%left '\n'

%%

Cmd   : if Exp then '\n' Cmd else '\n' Cmd end { IfThenElse $2 $5 $8 }
      | while Exp '\n' Cmd end                 { While $2 $4 }
      | var '=' Exp                            { Attr $1 $3 }
      | print Exp                              { PrintInt $2 }
      | print string                           { PrintString $2 }
      | read var                               { Read $2 }
      | Cmd '\n' Cmd                           { Seq $1 $3 }
      | Cmd '\n'                               { $1 }
      | '\n' Cmd                               { $2 }

Exp   : Exp '+' Exp                            { Operation $1 Add $3 }
      | Exp '-' Exp                            { Operation $1 Sub $3 }
      | Exp '*' Exp                            { Operation $1 Mul $3 }
      | Exp '/' Exp                            { Operation $1 Div $3 }
      | Exp '$' Exp                            { Operation $1 Eql $3 }
      | Exp '<' Exp                            { Operation $1 Lst $3 }
      | Exp '>' Exp                            { Operation $1 Grt $3 }
      | '(' Exp ')'                            { $2 }
      | int                                    { Const (TypeInt $1) }
      | bool                                   { Const (TypeBool $1) }
      | var                                    { Var $1 }
{

parseError :: [Token] -> a
parseError _ = error "Parse error"

data Symbol = Add
            | Sub
            | Mul
            | Div
            | Lst -- Less than
            | Grt -- Greater than
            | Eql deriving Show

data Value = TypeBool Bool
           | TypeInt Int deriving Show

data Expr = Const Value
          | Operation Expr Symbol Expr
          | Var String deriving Show

data State = Info [(String, Value)] [String] [String] deriving (Show)

data Command = IfThenElse Expr Command Command
             | While Expr Command 
             | Attr String Expr
             | PrintInt Expr
             | PrintString String
             | Read String
             | Seq Command Command deriving Show

data Token
      = TokenInt Int
      | TokenBool Bool
      | TokenVar String
      | TokenString String
      | TokenWhile
      | TokenIf
      | TokenThen
      | TokenElse
      | TokenEnd
      | TokenPrint
      | TokenRead
      | TokenAt
      | TokenEqual
      | TokenPlus
      | TokenMinus
      | TokenTimes
      | TokenDiv
      | TokenLess
      | TokenGreater
      | TokenOB
      | TokenCB
      | TokenLC deriving Show

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs)
  | c == '\n' = TokenLC : lexer cs
  | isSpace c = lexer cs
  | isAlpha c = lexVar (c:cs)
  | isDigit c = lexNum (c:cs)
lexer ('=':cs) = TokenAt : lexer cs
lexer ('$':cs) = TokenEqual : lexer cs
lexer ('+':cs) = TokenPlus : lexer cs
lexer ('-':cs) = TokenMinus : lexer cs
lexer ('*':cs) = TokenTimes : lexer cs
lexer ('<':cs) = TokenLess : lexer cs
lexer ('>':cs) = TokenGreater : lexer cs
lexer ('/':cs) = TokenDiv : lexer cs
lexer ('(':cs) = TokenOB : lexer cs
lexer (')':cs) = TokenCB : lexer cs
lexer ('"':cs) = lexString cs
lexer ('~':cs) = lexComment cs

lexNum cs = TokenInt (read num) : lexer rest
      where (num, rest) = span isDigit cs

lexComment cs = lexer rest
      where (num, rest) = break ('\n'==) cs

lexString cs = TokenString st : (lexer $ tail rest)
      where (st, rest) = break ('"'==) cs

lexVar cs =
   case span isAlphaNum cs of
     ("while", rest) -> TokenWhile : lexer rest
     ("if", rest) -> TokenIf : lexer rest
     ("then", rest) -> TokenThen : lexer rest
     ("else", rest) -> TokenElse : lexer rest
     ("end", rest) -> TokenEnd : lexer rest
     ("read", rest) -> TokenRead : lexer rest
     ("print", rest) -> TokenPrint : lexer rest
     ("true", rest) -> TokenBool True : lexer rest
     ("false", rest)  -> TokenBool False : lexer rest
     (var, rest)   -> TokenVar var : lexer rest

find :: [(String, Value)] -> String -> Value
find [] _ = TypeInt 0
find (x:xs) s
     | (fst x) == s = snd x
     | otherwise = find xs s

apply :: [(String, Value)] -> String -> Value -> [(String, Value)]
apply [] s vl = [(s, vl)]
apply (x:xs) s vl
     | (fst x) == s = (s, vl) : xs
     | otherwise = x : (apply xs s vl)

calc :: Symbol -> Value -> Value -> Value
calc Add (TypeInt a) (TypeInt b) = TypeInt (a + b)
calc Sub (TypeInt a) (TypeInt b) = TypeInt (a - b)
calc Mul (TypeInt a) (TypeInt b) = TypeInt (a * b)
calc Div (TypeInt a) (TypeInt b) = TypeInt (a `div` b)
calc Lst (TypeInt a) (TypeInt b) = TypeBool (a < b)
calc Grt (TypeInt a) (TypeInt b) = TypeBool (a > b)
calc Eql (TypeInt a) (TypeInt b) = TypeBool (a == b)

eval :: [(String, Value)] -> Expr -> Value
eval _ (Const a) = a
eval st (Operation a s b) = calc s (eval st a) (eval st b)
eval tbl (Var s) = find tbl s

castBool :: Value -> Bool
castBool (TypeInt a) = a /= 0
castBool (TypeBool a) = a

getVal :: Value -> String
getVal (TypeInt a) = show a
getVal (TypeBool a) = show a

interp :: State -> Command -> State
interp (Info tbl inp out) (IfThenElse exp cmdT cmdF)
       | castBool (eval tbl exp) = interp (Info tbl inp out) cmdF
       | otherwise = interp (Info tbl inp out) cmdT
interp (Info tbl inp out) (While exp cmd)
       | castBool (eval tbl exp) = interp (interp (Info tbl inp out) cmd) (While exp cmd)
       | otherwise = (Info tbl inp out)
interp (Info tbl inp out) (Attr str exp) = (Info (apply tbl str (eval tbl exp)) inp out)
interp st (Seq c1 c2) = interp (interp st c1) c2
interp (Info tbl inp out) (PrintInt exp) = (Info tbl inp (out ++ [getVal (eval tbl exp)]))
interp (Info tbl inp out) (PrintString s) = (Info tbl inp (out ++ [s]))
interp (Info tbl (arg:inp) out) (Read str) = (Info (apply tbl str (TypeInt ((read arg)::Int))) inp out)

getOutput :: State -> [String]
getOutput (Info _ _ out) = out

execute :: Command -> [String] -> [String]
execute cmd input = getOutput (interp (Info [] input []) cmd)
 
parse [] = getContents
parse fs = concat `fmap` mapM readFile fs

main = do
  code <- getArgs >>= parse
--  putStrLn $ show (go(lexer(code)))
  interact $ unlines . execute (go (lexer code)) . lines
}
