module Main
(
    main,
)
where

import Shared.Lexer
import Shared.Driver
import LambdaSimplyTyped.Parser
import LambdaSimplyTyped.TypeCheck
import LambdaSimplyTyped.Eval

main :: IO ()
main = do
    contents <- readFileArgument
    let stages = [ typecheck
                 , (liftEval eval)
                 ]
    let interpreter = Interpreter lexer parse stages
    let out = interpret contents interpreter
    print $ show out

