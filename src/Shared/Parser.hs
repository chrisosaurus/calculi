module Shared.Parser
(
    build_parser,
    parse_unit,
    parse_var,
    parse_single,
    parse_double,
    parse_triple,
    parse_abs,
    parse_app,
    ParserFunction,
)
where

import Control.Applicative
import Shared.Lexer
import Data.List (foldl1)

type ParserInner a = [Token] -> Either String (a, [Token])
type ParserFunction a = ParserInner a -> [Token] -> Maybe (a, [Token])

build_parser :: [ParserFunction a] -> [Token] -> Either String a
build_parser functions tokens = case parse_inner functions tokens of
                                     Left str               -> Left str
                                     Right (exp, [])        -> Right exp
                                     Right (exp, remaining) -> Left $ "Failed to completely parse: " ++ (stringify_tokens tokens)

parse_inner :: [ParserFunction a] -> ParserInner a
parse_inner functions tokens = case (foldl1 (<|>) applied_funcs) of
                                           Nothing               -> Left $ "Failed to parse: " ++ (stringify_tokens tokens)
                                           Just (exp, remaining) -> Right (exp, remaining)
    where applied_funcs = map (\f -> f (parse_inner functions) tokens) functions


-- Parses a Symbol with no recursive Exp
-- (Symbol string):rest
parse_unit :: String -> a -> ParserFunction a
parse_unit string ast_node _ ((Symbol string'):rest) | string == string' = Just (ast_node, rest)
parse_unit _ _ _ _ = Nothing

-- Parses any symbol
-- (Symbol _):rest
parse_var :: (String -> a) -> ParserFunction a
parse_var ast_node _ ((Symbol string):rest) = Just ((ast_node string), rest)
parse_var _ _ _ = Nothing

-- Parse a Symbol with one recursive Exp
-- ((Symbol string):e:rest)
parse_single :: String -> (a -> a) -> ParserFunction a
parse_single string ast_node pi (LParen:(Symbol string'):body) | string == string' =
    case pi body of
         Left _                   -> Nothing
         Right (e, (RParen:rest)) -> Just ((ast_node e), rest)
         Right _                  -> Nothing
parse_single _ _ _ _ = Nothing

-- Parse a Symbol with two recursive Exp
-- ((Symbol string):e:e:rest)
parse_double :: String -> (a -> a -> a) -> ParserFunction a
parse_double string ast_node pi (LParen:(Symbol string'):body) | string == string' =
    case pi body of
         Left _          -> Nothing
         Right (e, rest) -> case pi rest of
                                 Left _                     -> Nothing
                                 Right (e', (RParen:rest')) -> Just ((ast_node e e'), rest')
                                 Right _                    -> Nothing
parse_double _ _ _ _ = Nothing

-- Parse a Symbol with three recursive Exp
-- ((Symbol string):e:e:e:rest)
parse_triple :: String -> (a -> a -> a -> a) -> ParserFunction a
parse_triple string ast_node pi (LParen:(Symbol string'):body) | string == string' =
    case pi body of
         Left _          -> Nothing
         Right (e, rest) ->
             case pi rest of
                  Left _            -> Nothing
                  Right (e', rest') ->
                      case pi rest' of
                           Left _                     -> Nothing
                           Right (e'', RParen:rest'') -> Just ((ast_node e e' e''), rest'')
                           Right _                    -> Nothing
parse_triple _ _ _ _ = Nothing

-- abs need to be wrapped in parenthesis to avoid syntactic ugly
parse_abs :: (String -> a -> a) -> ParserFunction a
parse_abs ast_node pi (LParen:Lambda:(Symbol binding):Period:body) =
    case pi body of
         Left _                   -> Nothing
         Right (e, (RParen:rest)) -> Just (ast_node binding e, rest)
         Right _                  -> Nothing
parse_abs _ _ _ = Nothing

-- app need to be wrapped in parenthesis to avoid ambiguity
parse_app :: (a -> a -> a) -> ParserFunction a
parse_app ast_node pi (LParen:body) =
    case pi body of
         Left _          -> Nothing
         Right (e, rest) -> case pi rest of
                                 Left _                     -> Nothing
                                 Right (e', (RParen:rest')) -> Just ((ast_node e e'), rest')
                                 Right _                    -> Nothing
parse_app _ _ _ = Nothing

