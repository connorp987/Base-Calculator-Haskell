

--Variables
type Vars = String

--Arithmetic expressions
data AExpr = Var Vars | Const Integer
            | Add AExpr AExpr | Sub AExpr AExpr
            | Mul AExpr AExpr | Div AExpr AExpr
    deriving Show

--Boolean expressions
data BExpr = TT | FF -- the true and false constants
            | And BExpr BExpr | Or BExpr BExpr | Not BExpr -- boolean operations
            | Eql AExpr AExpr -- equality of arithmetic expressions
            | Lt AExpr AExpr -- true i f the f i r s t is less than the second
    deriving Show

type Env = [(Vars,Integer)]

-- Instructions
data Instr = Assign Vars AExpr -- assign X to the value of an expression
            | Seq Instr Instr
            | IfThenElse BExpr Instr Instr -- conditional
            | While BExpr Instr -- looping construct
            | Do [ Instr ] -- opening a block of several instructions
            | Nop -- the "do nothing" instruction
    deriving Show
-- A program is a l i s t of instructions
type Program = [ Instr ]

lookUp :: String -> Env -> Integer
lookUp x [] = undefined
lookUp x ( (y,v) : yvs) | x == y   = v
                        | otherwise = lookUp x yvs

fromJust :: Maybe a -> a
fromJust (Just x) = x
fromJust Nothing = undefined


evala :: Env -> AExpr -> Integer
evala e (Var x) = fromJust (lookup x e)
evala e (Const y) = y
evala e (Add p1 p2) = evala e p1 + evala e p2
evala e (Sub p1 p2) = evala e p1 - evala e p2
evala e (Mul p1 p2) = evala e p1 * evala e p2
evala e (Div p1 p2) = evala e p1 `div` evala e p2


evalb :: Env -> BExpr -> Bool
evalb e (TT) = True
evalb e (FF) = False
evalb e (And p1 p2) = evalb e p1 && evalb e p2
evalb e (Or p1 p2) = evalb e p1 || evalb e p2
evalb e (Not p) = not (evalb e p)
evalb e (Eql p1 p2) = evala e p1 == evala e p2
evalb e (Lt p1 p2) = evala e p1 < evala e p2

update :: String -> Integer -> Env -> Env
update x newval [] = [(x,newval)]
update x newval ((y,v) : e) | x == y = (x,newval) : e
                            | otherwise = (y,v) : update x newval e

insert :: Vars -> Integer -> Env -> Env
insert x i [] = [(x,i)]
insert x i ( (y,v) : yvs)   | x == y   = (x,i) : yvs
                            | x < y = (x,i) : (y,v) : yvs
                            | x > y = (y,v) : insert x i yvs

doWhile :: [Instr] -> Env -> Env
doWhile [] m = m
doWhile (x:y) m = doWhile y (exec x m)

exec :: Instr -> Env -> Env
exec (Seq s1 s2) m = exec s2 (exec s1 m)
exec (Assign x e) m = insert x (evala m e) m
exec (IfThenElse e p1 p2) m = if evalb m e then exec p1 m else exec p2 m
exec Nop m = m
exec (Do e) m = doWhile e m
exec (While e s) m = exec (IfThenElse e (Seq s (While e s)) Nop) m 


data UOps = NotOp deriving Show
data BOps = AddOp | SubOp | MulOp | DivOp | AndOp | OrOp | EqlOp | LtOp | AssignOp
    deriving Show
data Token = VSym String | CSym Integer | BSym Bool
            | UOp UOps | BOp BOps
            | LPar | RPar | Semi
            | Keyword String
            | Err
            | PA AExpr | PB BExpr | PI Instr
    deriving Show

isAlpha :: Char -> Bool
isAlpha x = ('A' <= x && x <= 'Z') || ('a' <= x && x <= 'z')

isDigit :: Char -> Bool
isDigit x = '0' <= x && x <= '9'

isCSym :: String -> Bool
isCSym "" = False
isCSym ('-':(x:xs)) | isDigit x = isCSym (x:xs)
isCSym (x:xs) =
  let q1 "" = True
      q1 (y:ys) | isDigit y = q1 ys
      q1 ('.':(y:ys)) | isDigit y = q3 ys
      q1 _ = False
      q3 "" = True
      q3 (y:ys) | isDigit y = q3 ys
      q3 _ = False
   in isDigit x && q1 xs

isVSym :: String -> Bool
isVSym "" = False
isVSym (x:xs) =
  let q1 "" = True
      q1 (y:ys) = if isDigit y || isAlpha y || y == ':'
                    then q1 ys
                    else if y == '\'' then q2 ys else False
      q2 "" = True
      q2 ('\'':ys) = q2 ys
      q2 _ = False
  in  isAlpha x && q1 xs

classify :: String -> Token
classify "(" = LPar
classify ")" = RPar
classify "*" = BOp MulOp
classify "+" = BOp AddOp
classify "-" = BOp SubOp
classify "/\\" = BOp AndOp
classify "\\/" = BOp OrOp
classify "<" = BOp LtOp
classify "!"   = UOp NotOp
classify "/" = BOp DivOp
classify "==" = BOp EqlOp
classify ":=" = BOp AssignOp
classify ";" = Semi
classify "else" = Keyword "else"
classify "T" = BSym True
classify "F" = BSym False
classify "while" = Keyword "while"
classify "if" = Keyword "if"
classify "then" = Keyword "then"
classify "else" = Keyword "else"
classify "do" = Keyword "do"
classify "nop" = Keyword "nop"
classify x | isCSym x = CSym (read x)
classify x | isVSym x = VSym x
classify _ = Err

addSpaces :: String -> String
addSpaces "" = ""
addSpaces ('*' : s) = " * " ++ addSpaces s
addSpaces ('+' : s) = " + " ++ addSpaces s
addSpaces ('-' : s) = " - " ++ addSpaces s
addSpaces ('/' : '\\' : s) = " /\\ " ++ addSpaces s
addSpaces ('/' : s) = " / " ++ addSpaces s
addSpaces ('(' : s) = " ( " ++ addSpaces s
addSpaces (')' : s) = " ) " ++ addSpaces s
addSpaces ('=' : '=' : s) = " == " ++ addSpaces s
addSpaces (':' : '=' : s) = " := " ++ addSpaces s
addSpaces ('\\' : '/' : s) = " \\/ " ++ addSpaces s
addSpaces ('<' : s) = " < " ++ addSpaces s
addSpaces ('!' : s) = " ! " ++ addSpaces s
addSpaces (';' : s) = " ; " ++ addSpaces s
addSpaces (x : s) = x : addSpaces s

lexer :: String -> [Token]
lexer s = map classify (words (addSpaces s))

parser :: [Token] -> Instr
parser l = sr [] ((Keyword "do" : LPar : l) ++ [RPar])

sr :: [Token] -> [Token] -> Instr
sr (CSym x : ts) i = sr (PA (Const x) : ts) i
sr (VSym x : ts) i = sr (PA (Var x) : ts) i
sr (BSym False : ts ) i = sr (PB FF : ts ) i
sr (BSym True  : ts ) i = sr (PB TT : ts ) i
sr (PA p2 : BOp DivOp : PA p1 : ts ) i = sr (PA (Div p1 p2) : ts ) i
sr (PA p2 : BOp AddOp : PA p1 : ts) i = sr (PA (Add p1 p2) : ts) i
sr (PA p2 : BOp SubOp : PA p1 : ts) i = sr (PA (Sub p1 p2) : ts) i
sr (PA p2 : BOp MulOp : PA p1 : ts) i = sr (PA (Mul p1 p2) : ts) i
sr (PA p2 : BOp LtOp : PA p1 : ts ) i = sr (PB (Lt p1 p2) : ts ) i
sr (PA p2 : BOp EqlOp : PA p1 : ts) l = sr (PB (Eql p1 p2) : ts) l
sr (PB p2 : BOp AndOp : PB p1 : ts) i = sr (PB (And p1 p2) : ts) i
sr (PB p2 : BOp OrOp : PB p1 : ts) i = sr (PB (Or p1 p2) : ts) i
sr (PB p : UOp NotOp : ts) i         = sr (PB (Not p) : ts) i
sr (PA p : BOp AssignOp : PA (Var x) : ts) l = sr (PI (Assign x p) : ts) l
sr (RPar : PA p : LPar : ts ) i = sr (PA p : ts ) i
sr (RPar : PI p : LPar : ts ) i = sr (PI p : ts ) i
sr (RPar : PB p : LPar : ts ) i = sr (PB p : ts ) i
sr (PI (Do is ) : Keyword "do" : ts ) l = sr (PI (Do is ) : ts ) l
sr (PI i2 : PB i1 : Keyword "while" : ts ) l = sr (PI (While i1 i2 ) : ts ) l
sr (Keyword "nop" : ts ) l = sr (PI (Nop) : ts ) l
sr (PI i2 : Keyword "else" : PI i1 : Keyword "then" : PB b : Keyword "if" : ts ) l = sr (PI (IfThenElse b i1 i2 ) : ts ) l
sr (PI (Do is ) : Semi : PI i : ts ) l = sr (PI (Do ( i : is )) : ts ) l
sr (RPar : PI p : ts ) i = sr (PI (Do [p] ) : ts ) (RPar : i )
sr xs (i:is) = sr (i:xs) is
sr [PI i] [] = i
sr s [] = error (show s)

run :: Program -> Env
run p = exec (Do p) [ ]

fibonacci8 :: String
fibonacci8 = "x:=0; \
\ y:=1; \
\ c:=0; \
\ while (c<7) \\/ (c==7) do ( \
\ c:= (c+1); \
\ z:= (x+y); \
\ x:=y; \
\ y:=z ) "

fibLexed :: [Token]
fibLexed = lexer fibonacci8

fibParsed :: Instr
fibParsed = parser fibLexed

fibResult :: Integer
fibResult = lookUp "x" (run [fibParsed])

factorial5 :: String
factorial5 =
    " fact:=1; \
    \ c :=1 ; \
    \ while (! (5 < c)) do ( \
    \ c := (c+1); \
    \ fact := (fact*c )\
    \ )"
factLexed = lexer factorial5
factParsed = parser ((LPar : factLexed) ++ [RPar])
factResult = lookUp "fact" (run [factParsed])