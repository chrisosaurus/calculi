module LambdaSimplyTyped.Parser
(
    parse
)
where

import Shared.Lexer
import Shared.Parser
import LambdaSimplyTyped.Data.Exp

parse = build_parser [
    parse_unit  "true"  ExpTrue,
    parse_unit  "false" ExpFalse,
    parse_unit  "unit"  ExpUnit,
    typed_parse_abs,
    parse_triple "if"   IfElse,
    parse_var           Var,
    parse_app           App
                     ]

typed_parse_abs :: ParserFunction Exp
typed_parse_abs pi (LParen:Lambda:(Symbol binding):Colon:rest) =
    case parse_simple_type rest of
        Nothing                   -> Nothing
        Just (simple_type, body)  ->
            case pi body of
                 Left _                    -> Nothing
                 Right (e, (RParen:rest')) -> Just ((Abs binding simple_type e), rest')
                 Right _                   -> Nothing
typed_parse_abs _  _ = Nothing

parse_simple_type :: [Token] -> Maybe (SimpleType, [Token])
parse_simple_type ((Symbol "Bool"):Arrow:rest) =
    case parse_simple_type rest of
        Nothing             -> Just (BoolType, rest)
        Just (right, rest') -> Just ((FuncType BoolType right), rest')
parse_simple_type ((Symbol "Unit"):Arrow:rest) =
    case parse_simple_type rest of
        Nothing             -> Just (UnitType, rest)
        Just (right, rest') -> Just ((FuncType UnitType right), rest')
parse_simple_type ((Symbol "Bool"):Period:rest) = Just (BoolType, rest)
parse_simple_type ((Symbol "Unit"):Period:rest) = Just (UnitType, rest)
parse_simple_type _ = Nothing

