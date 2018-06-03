module LambdaUntyped.Parser
(
    parse
)
where

import Shared.Parser
import LambdaUntyped.Data.Exp

parse = build_parser [
    parse_unit  "true"  ExpTrue,
    parse_unit  "false" ExpFalse,
    parse_unit  "unit"  Unit,
    parse_abs           Abs,
    parse_triple "if"   IfElse,
    parse_var           Var,
    parse_app           App
                     ]

