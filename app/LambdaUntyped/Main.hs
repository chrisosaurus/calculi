module Main
(
    main,
)
where

import Shared.Lexer
import Shared.Driver
import LambdaUntyped.Parser
import LambdaUntyped.Eval

main :: IO ()
main = do
    contents <- readFileArgument
    let interpreter = Interpreter lexer parse [(liftEval eval)]
    let out = interpret contents interpreter
    putStrLn $ showResult out

