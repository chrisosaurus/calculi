module LambdaSystemF.Parser
(
    parse
)
where

import Shared.Lexer
import Shared.Parser
import LambdaSystemF.Data.Exp

parse = build_parser [
    parse_unit  "true"  ExpTrue,
    parse_unit  "false" ExpFalse,
    parse_unit  "unit"  ExpUnit,
    parse_systemf_abs,
    parse_systemf_type_abs,
    parse_systemf_type_app,
    parse_triple "if"   IfElse,
    parse_var           Var,
    parse_app           App
                     ]

parse_systemf_abs :: ParserFunction Exp
parse_systemf_abs pi (LParen:Lambda:(Symbol binding):Colon:rest) =
    case parse_systemf_type rest of
        Nothing                   -> Nothing
        Just (systemf_type, (Period:body))  ->
            case pi body of
                 Left _                    -> Nothing
                 Right (e, (RParen:rest')) -> Just ((Abs binding systemf_type e), rest')
                 Right _                   -> Nothing
parse_systemf_abs _  _ = Nothing

parse_systemf_type :: [Token] -> Maybe (SystemFType, [Token])
parse_systemf_type ((Symbol "Bool"):Arrow:rest) =
    case parse_systemf_type rest of
        Nothing             -> Nothing
        Just (right, rest') -> Just ((FuncType BoolType right), rest')
parse_systemf_type ((Symbol "Unit"):Arrow:rest) =
    case parse_systemf_type rest of
        Nothing             -> Nothing
        Just (right, rest') -> Just ((FuncType UnitType right), rest')
parse_systemf_type ((Symbol "Bool"):rest) = Just (BoolType, rest)
parse_systemf_type ((Symbol "Unit"):rest) = Just (UnitType, rest)
parse_systemf_type ((Symbol "forall"):(Symbol var):Period:rest) =
    case parse_systemf_type rest of
        Nothing             -> Nothing
        Just (right, rest') -> Just ((UniversalType var right), rest')
parse_systemf_type ((Symbol var):rest) = Just ((TypeVariable var), rest)
parse_systemf_type _ = Nothing

parse_systemf_type_abs :: ParserFunction Exp
parse_systemf_type_abs pi (LParen:LAMBDA:(Symbol binding):Period:body) =
    case pi body of
        Left _                   -> Nothing
        Right (e, (RParen:rest)) -> Just ((TypeAbs binding e), rest)
        Right _                  -> Nothing
parse_systemf_type_abs _ _ = Nothing

-- TODO FIXME app needs to be wrapped in parens
parse_systemf_type_app :: ParserFunction Exp
parse_systemf_type_app pi (LParen:body) =
    case pi body of
        Left _                   -> Nothing
        Right (e, (LBrack:rest)) ->  case parse_systemf_type rest of
                                        Nothing                                    -> Nothing
                                        Just (systemf_type, (RBrack:RParen:rest')) -> Just ((TypeApp e systemf_type), rest')
                                        Just _                                     -> Nothing
        Right _                  -> Nothing
parse_systemf_type_app _ _ = Nothing

