module Main
(
    main,
)
where

import Shared.Lexer
import Shared.Driver
import LambdaAssignment.Parser
import LambdaAssignment.Eval

main :: IO ()
main = do
    contents <- readFileArgument
    let interpreter = Interpreter lexer parse [(liftEval eval)]
    let out = interpret contents interpreter
    print $ show out
