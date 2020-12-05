

-- Type of polynomials
data Poly a = Var a | Const Double | Add (Poly a) (Poly a)
                    | Mul (Poly a) (Poly a) | Sub (Poly a) (Poly a)

-- 3x^2+2
-- Add (Mul (Const 3.0) (Mul (Var "X") (Var "X"))) (Const 2.0)

-- 3(x+2)
-- Mul (Const 3.0) (Add (Var "X") (Const 2.0))

isAlpha :: Char -> Bool
isAlpha x = ('A' <= x && x <= 'Z') || ('a' <= x && x <= 'z')

isDigit :: Char -> Bool
isDigit x = '0' <= x && x <= '9'

-- The previous definition, below, is no longer sufficient.
-- isVSym :: String -> Bool
-- isVSym "" = False
-- isVSym (x:xs) = isAlpha x && all (\y -> isAlpha y || isDigit y) xs

-- Rules for variable names:
-- 1. Start with a letter
-- 2. Contain - or _ in the name
-- 3. Can include any number of apostrophes ' at the end

-- The variable checker implements the FSA for variables
isVSym :: String -> Bool
isVSym "" = False
isVSym (x:xs) =
  let q1 "" = True
      q1 (y:ys) = if isDigit y || isAlpha y || y == '-' || y == '_'
                    then q1 ys
                    else if y == '\'' then q2 ys else False
      q2 "" = True
      q2 ('\'':ys) = q2 ys
      q2 _ = False
  in  isAlpha x && q1 xs

-- The constant checker implements the FSA for constants
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

data BOps = AddOp | MulOp | SubOp deriving Show
data Token = VSym String | CSym Double | BOp BOps | LPar | RPar | Err deriving Show

classify :: String -> Token
classify "(" = LPar
classify ")" = RPar
classify "*" = BOp MulOp
classify "+" = BOp AddOp
classify "-" = BOp SubOp
classify x | isCSym x = CSym (read x)
classify x | isVSym x = VSym x
classify _ = Err

lexer :: String -> [Token]
lexer s = map classify (words s)