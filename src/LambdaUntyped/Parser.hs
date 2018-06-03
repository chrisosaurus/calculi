module LambdaUntyped.Parser
(
    parse
)
where

import Control.Applicative
import Shared.Lexer
import LambdaUntyped.Data.Exp

parse :: [Token] -> Either String Exp
parse [] = Left "no input provided to parse"
parse tokens = case parse_inner tokens of
                    Left str               -> Left str
                    Right (exp, [])        -> Right exp
                    Right (exp, remaining) -> Left $ "Failed to parse: " ++ (stringify_tokens tokens)

parse_inner :: [Token] -> Either String (Exp, [Token])
parse_inner [] = Left "no input provided to parse"
parse_inner tokens =
    case     parse_lit   tokens
         <|> parse_abs   tokens
         <|> parse_if    tokens
         <|> parse_var   tokens
         <|> parse_app   tokens
         of  Nothing               -> Left $ "Failed to parse: " ++ (stringify_tokens tokens)
             Just (exp, remaining) -> Right (exp, remaining)

parse_lit :: [Token] -> Maybe (Exp, [Token])
parse_lit ((Symbol "true") :rest) = Just (ExpTrue, rest)
parse_lit ((Symbol "false"):rest) = Just (ExpFalse, rest)
parse_lit ((Symbol "unit") :rest) = Just (Unit, rest)
parse_lit _                       = Nothing

parse_var :: [Token] -> Maybe (Exp, [Token])
parse_var ((Symbol sym):rest) = Just (Var sym, rest)
parse_var _                   = Nothing

-- abs need to be wrapped in parenthesis to avoid syntactic ugly
parse_abs :: [Token] -> Maybe (Exp, [Token])
parse_abs (LParen:(Lambda:(Symbol binding):Period:body)) =
    case parse_inner body of
         Left _                   -> Nothing
         Right (e, (RParen:rest)) -> Just (Abs binding e, rest)
         Right _                  -> Nothing
parse_abs _ = Nothing

-- app need to be wrapped in parenthesis to avoid ambiguity
parse_app :: [Token] -> Maybe (Exp, [Token])
parse_app (LParen:body) =
    case parse_inner body of
         Left _          -> Nothing
         Right (e, rest) -> case parse_inner rest of
                                 Left _                     -> Nothing
                                 Right (e', (RParen:rest')) -> Just ((App e e'), rest')
                                 Right _                    -> Nothing

parse_if :: [Token] -> Maybe (Exp, [Token])
parse_if (LParen:(Symbol "if"):body) =
    case parse_inner body of
         Left _          -> Nothing
         Right (e, rest) ->
             case parse_inner rest of
                  Left _            -> Nothing
                  Right (e', rest') ->
                      case parse_inner rest' of
                           Left _                     -> Nothing
                           Right (e'', RParen:rest'') -> Just ((IfElse e e' e''), rest'')
                           Right _                    -> Nothing
parse_if _ = Nothing


