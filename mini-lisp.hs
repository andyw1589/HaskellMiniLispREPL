import Text.Read
import Text.Parsec as Parsec
import Text.Parsec.String

-- Mini-lisp is a small lisp-inspired PL that does simple operations on integers, every expression should evaluate to a number
-- Expanded from a small exercise done in CSC324 to include a REPL and text parser, so you need to download parsec
-- https://hackage.haskell.org/package/parsec
-- Syntax:
-- <expr> = <n>  (a literal number)
--        | <id>  (a variable storing a number, evals to Nothing if variable isn't bound)
--        | (set <id> <expr>)  (binds variable <id> to value <expr> and evaluates to the value)
--                             (NOTE: <id> must only be letters)
--        | (+ <expr> <expr>)  (adds two expressions)
--        | (- <expr> <expr>)  (subtracts the second expression from the first)
--        | (* <expr> <expr>)  (multiplies two expressions)
-- (C) Andy Wang 24/6/23

-- Type representing variable-value bindings
type Environment = [(String, Int)]

-- Data for AST
data Expr = Lit String | Var String | Set String Expr | Add Expr Expr | Sub Expr Expr | Mult Expr Expr
instance Show Expr where
    show (Lit a) = "(Lit " ++ a ++ ")"
    show (Var a) = "(Var " ++ a ++ ")"
    show (Set s expr) = "(Set " ++ s ++ " " ++ (show expr) ++ ")"
    show (Add e1 e2) = "(Add " ++ (show e1) ++ " " ++ (show e2) ++ ")"
    show (Sub e1 e2) = "(Sub " ++ (show e1) ++ " " ++ (show e2) ++ ")"
    show (Mult e1 e2) = "(Mult " ++ (show e1) ++ " " ++ (show e2) ++ ")"

-- Get the current environment
getEnv :: Environment -> (Environment, Environment)
getEnv env = (env, env)

-- Update the environment
updateEnv :: Environment -> Environment -> (Environment, Environment)
updateEnv env _ = (env, env)

-- Given a value and environment, return it as a pair (enter monad ecosystem)
wrap :: Maybe Int -> Environment -> (Maybe Int, Environment)
wrap val env = (val, env)

-- Monadic binding
bind :: (Environment -> (a, Environment)) -> (a -> Environment -> (b, Environment)) -> Environment -> (b, Environment)
bind op1 op2 env0 =
  let (res, env1) = op1 env0
  in op2 res env1

-- Assigns a value to a variable, puts it at the head of the environment
assign :: Environment -> String -> Int -> Environment
assign env var val = (var, val):env

-- Look up a variable's value, return Nothing if it's not found
lookUp :: Environment -> String -> Maybe Int
lookUp [] _ = Nothing
lookUp ((x, val):xs) id =
  if x == id
    then Just val
    else lookUp xs id

-- Helper function for binary operations
mathOp :: (Int -> Int -> Int) -> Maybe Int -> Maybe Int -> Maybe Int
mathOp _ _ Nothing = Nothing
mathOp _ Nothing _ = Nothing
mathOp op (Just x) (Just y) = Just (op x y)

-- The main evaluation function
eval :: Expr -> Environment -> (Maybe Int, Environment)
-- binary math operations
eval (Add expr1 expr2) =
    eval expr1 `bind` \v1 ->
        eval expr2 `bind` \v2 ->
            wrap (mathOp (+) v1 v2)
eval (Sub expr1 expr2) =
    eval expr1 `bind` \v1 ->
        eval expr2 `bind` \v2 ->
            wrap (mathOp (-) v1 v2)
eval (Mult expr1 expr2) =
    eval expr1 `bind` \v1 ->
        eval expr2 `bind` \v2 ->
            wrap (mathOp (*) v1 v2)

-- Set new binding
eval (Set id expr) =
    eval expr `bind` \val ->
        case val
        of
            Nothing -> wrap Nothing
            Just n -> getEnv `bind` \env ->
                updateEnv ((id, n):env) `bind` \_ ->
                    wrap (Just n)

eval (Var id) =
    getEnv `bind` (\env -> wrap (lookUp env id))
eval (Lit n) = wrap (readMaybe n)

-- Parse string into AST
parser :: Parser Expr
parser = parseLit <|> parseVar <|> (string "(" >> (parseSet <|> parseAdd <|> parseSub <|> parseMult))

parseLit :: Parser Expr
parseLit = do
    digits <- many1 digit
    return (Lit digits)

parseVar :: Parser Expr
parseVar = do
    letters <- many1 letter
    return (Var letters)

parseSet :: Parser Expr
parseSet = do
    string "set"
    spaces
    name <- many1 letter
    spaces
    expr <- parser
    char ')'
    return (Set name expr)

createMathOpParser :: Char -> (Expr -> Expr -> Expr) -> Parser Expr
createMathOpParser op typeConstructor = do
    char op
    spaces
    expr1 <- parser
    spaces
    expr2 <- parser
    char ')'
    return (typeConstructor expr1 expr2)

parseAdd :: Parser Expr
parseAdd = createMathOpParser '+' (Add)

parseSub :: Parser Expr
parseSub = createMathOpParser '-' (Sub)

parseMult :: Parser Expr
parseMult = createMathOpParser '*' (Mult)

-- Parsing returns an Either instance, helper function to handle either case
sanitizeParseAndEval :: Either ParseError Expr -> Environment -> (Maybe Int, Environment)
sanitizeParseAndEval (Left _) env = (Nothing, env)
sanitizeParseAndEval (Right ast) env = eval ast env

-- The main driver code for the REPL
repl :: Environment -> IO String
repl env = do
    input <- getLine  -- read
    let result = parse parser "" input
    let (res, newEnv) = sanitizeParseAndEval result env  -- eval
    print res  -- print
    repl newEnv  -- loop

main :: IO String
main = repl []
