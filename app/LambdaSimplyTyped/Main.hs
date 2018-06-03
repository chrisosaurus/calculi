module Main
(
    main,
)
where

import System.Environment
import Shared.Lexer
import LambdaSimplyTyped.Parser
import LambdaSimplyTyped.Eval

main :: IO ()
main = do
    args <- getArgs
    if length args == 0 || length args > 1
    then print "Please provide a single argument which is the filename"
    else do
        let (filename:[]) = args
        contents <- readFile filename
        case lexer contents of
             Left err     -> print err
             Right tokens -> do
                case parse tokens of
                     Left err  -> print err
                     Right exp -> print $ show $ eval exp
