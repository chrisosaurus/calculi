module Shared.Driver
(
    readFileArgument,
    Interpreter (..),
    interpret,
    liftEval,
    showResult,
)
where

import System.Environment

-- attempt to get a filename provided as an argument
getFilename :: IO String
getFilename = do
    args <- getArgs
    if length args == 0 || length args > 1
    then fail "Please provide a single argument which is the filename"
    else return (head args)

-- attempt to get filename provided as an argument
-- then attempt to read content of file
readFileArgument :: IO String
readFileArgument = do
    filename <- getFilename
    contents <- readFile filename
    return contents

type StageFunction exp = exp -> Either String exp

-- an interpreter is made up of a lexer, a parser, and then a set of 'stages'
-- to run.
--
-- these stages could be type checkers or even an evaluation stage
data Interpreter token exp = Interpreter
    { lexerFunc  :: String  -> Either String [token]
    , parserFunc :: [token] -> Either String exp
    , stages     :: [StageFunction exp]
    }

-- interpret will feed the input into the interpreter and run through all the
-- stages, stopping at the first error, or when the final stage is complete
interpret :: String -> Interpreter token exp -> Either String exp
interpret contents interpreter =
    case (lexerFunc interpreter) contents of
        Left  err -> Left err
        Right tokens -> case (parserFunc interpreter) tokens of
            Left  err -> Left err
            Right e -> foldStages e (stages interpreter)

foldStages :: exp -> [StageFunction exp] -> Either String exp
foldStages e [] = Right e
foldStages e (x:xs) = case x e of
                           Left  err -> Left err
                           Right e'  -> foldStages e' xs

-- take a function (e -> e) and lift it into (e -> Either String e)
liftEval :: (exp -> exp) -> (exp -> Either String exp)
liftEval eval = \e -> Right (eval e)

showResult :: Show exp => Either String exp -> String
showResult (Left  err) = "ERROR: " ++ err
showResult (Right exp) = show exp
