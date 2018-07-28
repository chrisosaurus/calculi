module Main
(
    main,
)
where

import Shared.Lexer
import Shared.Driver
import LambdaSystemF.Parser
import LambdaSystemF.TypeCheck
import LambdaSystemF.Eval

main :: IO ()
main = do
    contents <- readFileArgument
    let stages = [ typecheck
                 , (liftEval eval)
                 ]
    let interpreter = Interpreter lexer parse stages
    let out = interpret contents interpreter
    putStrLn $ showResult out

