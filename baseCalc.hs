--Advanced Base Calculator

--Connor Plunkett and Zack Noble (https://github.com/connorp987 and https://github.com/zanoble)

--Variables
type Vars = String

--Computation 
--data Comp = Const Integer
--            | Add Comp Comp | Sub Comp Comp
--            | Mul Comp Comp | Div Comp Comp
--    deriving Show

data Poly a = Var a | Const Integer | Add (Poly a) (Poly a)
                    | Mul (Poly a) (Poly a) | Sub (Poly a) (Poly a)
                    | Div (Poly a) (Poly a)
                    | Sin (Poly a) | Cos (Poly a)
  deriving Show

type Base = Integer

type Env = [(String,Integer)]

lookUp :: String -> Env -> Integer
lookUp x ((y,v) : e) | x == y = v
                     | otherwise = lookUp x e

lookUpDec :: Integer -> String
lookUpDec x | x == 10 = "A"
            | x == 11 = "B"
            | x == 12 = "C"
            | x == 13 = "D"
            | x == 14 = "E"
            | x == 15 = "F"
            | otherwise = show x

convBase :: Integer -> Base -> String
convBase x 1 = show x
convBase x b | b < 2 = error "Base cannot be less then two"
             | x < b = lookUpDec x
             | otherwise =
               let f y = if y < b then lookUpDec y else lookUpDec (mod y b) ++ f (div y b)
               in reverse (f x)

evalP :: Env -> Poly String -> Base-> Integer
evalP env (Var x) base   = lookUp x env
evalP env (Const v) base =  v
evalP env (Add p1 p2) base = evalP env p1 base + evalP env p2 base
evalP env (Sub p1 p2) base = evalP env p1 base - evalP env p2 base
evalP env (Mul p1 p2) base = evalP env p1 base * evalP env p2 base
evalP env (Div p1 p2) base = div (evalP env p1 base) (evalP env p2 base)


data BOps = AddOp | MulOp | SubOp | DivOp deriving Show
--data UOps = SinOp | CosOp deriving Show
data Token = VSym String | CSym Integer | BOp BOps
           | LPar | RPar | Err
           | PP (Poly String)
  deriving Show

isCSym :: String -> Bool
isCSym "" = False
isCSym ('-':(x:xs)) | isDigitP x = isCSym (x:xs)
isCSym (x:xs) =
  let q1 "" = True
      q1 (y:ys) | isDigitP y = q1 ys
      q1 ('.':(y:ys)) | isDigitP y = q3 ys
      q1 _ = False
      q3 "" = True
      q3 (y:ys) | isDigitP y = q3 ys
      q3 _ = False
   in isDigitP x && q1 xs

isAlphaP :: Char -> Bool
isAlphaP x = ('G' <= x && x <= 'Z') || ('a' <= x && x <= 'z')

isDigitP :: Char -> Bool
isDigitP x = ('0' <= x && x <= '9') || ('A' <= x && x <= 'F')

isVSym :: String -> Bool
isVSym "" = False
isVSym (x:xs) =
  let q1 "" = True
      q1 (y:ys) = if isDigitP y || isAlphaP y || y == '-' || y == '_'
                    then q1 ys
                    else if y == '\'' then q2 ys else False
      q2 "" = True
      q2 ('\'':ys) = q2 ys
      q2 _ = False
  in  isAlphaP x && q1 xs

len :: String -> Integer
len "" = 0
len (x:xs) = 1 + len xs

expt :: Integer -> Integer -> Integer
expt b 0 = 1
expt b n = b * expt b (n-1)

lookAlpha :: String -> Integer
lookAlpha s | s == "A" = 10
            | s == "B" = 11
            | s == "C" = 12
            | s == "D" = 13
            | s == "E" = 14
            | s == "F" = 15
            | otherwise =  read s

readBase :: String -> Base -> Integer
readBase "" b = 0
readBase (x:xs) b = lookAlpha [x] * expt b (len xs) + readBase xs b

classify :: Base -> String -> Token
classify b "(" = LPar
classify b ")" = RPar
classify b "*" = BOp MulOp
classify b "/" = BOp DivOp
classify b "+" = BOp AddOp
classify b "-" = BOp SubOp
classify b x | isCSym x = CSym (readBase x b)
classify b x | isVSym x = VSym x
classify b _ = Err

-- A "preprocessor" stage adds enough whitespaces around the operators
addSpaces :: String -> String
addSpaces "" = ""
addSpaces ('*' : s) = " * " ++ addSpaces s
addSpaces ('/' : s) = " / " ++ addSpaces s
addSpaces ('+' : s) = " + " ++ addSpaces s
addSpaces ('-' : s) = " - " ++ addSpaces s
addSpaces ('(' : s) = " ( " ++ addSpaces s
addSpaces (')' : s) = " ) " ++ addSpaces s
addSpaces (x : s) = x : addSpaces s

lexer :: Base -> String -> [Token]
lexer b s = map (classify b) (words (addSpaces s))

sr :: [Token] -> [Token] -> Poly String
sr (CSym x : ts) i = sr (PP (Const x) : ts) i
sr (VSym x : ts) i = sr (PP (Var   x) : ts) i
sr (PP p2 : BOp MulOp : PP p1 : ts) i = sr (PP (Mul p1 p2) : ts) i
sr (PP p2 : BOp DivOp : PP p1 : ts) i = sr (PP (Div p1 p2) : ts) i
sr (PP p2 : BOp AddOp : PP p1 : ts) i = sr (PP (Add p1 p2) : ts) i
sr (PP p2 : BOp SubOp : PP p1 : ts) i = sr (PP (Sub p1 p2) : ts) i
sr (RPar : PP p : LPar : ts) i        = sr (PP p : ts) i
sr xs (i:is) = sr (i:xs) is

sr [PP p] [] = p
parser :: [Token] -> Poly String
parser l = sr [] l

update :: String -> Integer -> Env -> Env
update x newval [] = [(x,newval)]
update x newval ((y,v) : e) | x == y = (x,newval) : e
                            | otherwise = (y,v) : update x newval e


-- A pretty-printer for environments
printEnv :: Env -> Base -> String
printEnv e b =
  let aux [] = ""
      aux [(x,v)] = x ++ " = " ++ convBase v b
      aux ((x,v) : e) = x ++ " = " ++ convBase v b ++ ", " ++ aux e
  in "[ " ++ aux e ++ "]"


main :: IO ()
main =
  let loop env base = do
      putStrLn "For more commands type \"help\". Otherwise:"
      putStrLn "Enter your input:"
      rawinput <- getLine
      case (words rawinput) of
        [] -> return ()
        ["exit"] -> return ()
        [":q"] -> return ()
        ["vars"] -> do
          putStrLn "The current values of the variables are: "
          putStrLn (printEnv env base)
          loop env base
        ["help"] -> do
          putStrLn "To add a variable do: \"x := 5+5\". Would result in x being stored as 10"
          putStrLn "\"vars\" - to display all variables"
          putStrLn "\"Base\" - to change the currenct Base"
          putStrLn "\"CurrentBase\" - to display current Base"
          putStrLn "\"exit\" or \":q\" - to exit program \n"
          loop env base
        ["CurrentBase"] -> do
          putStrLn "The current value of the Base is: "
          putStrLn (show base)
          loop env base
        ["Base"] -> do
          putStrLn "Enter New base:"
          newBase <- getLine
          let baseNum = read newBase
          putStrLn ("New Base: " ++ newBase)
          loop env baseNum
        (v : ":=" : pe) -> do
          let p = parser (lexer base (unwords pe))
          let r = evalP env p base
          putStrLn ("The variable is set to new value: " ++ convBase r base ++ "\n")
          loop (update v r env) base -- The loop is called recursively with NEW env
        expr -> do
          let p = parser (lexer base (unwords expr))
          let r = evalP env p base
          putStrLn ("Your expression evaluates to: " ++ convBase r base ++ "\n")
          loop env base    -- The loop is called recursively with the same env
  in loop [] 10