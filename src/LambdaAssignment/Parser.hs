module LambdaAssignment.Parser
(
    parse
)
where

import Shared.Parser
import LambdaAssignment.Data.Exp

parse = build_parser [
    parse_unit   "true"  ExpTrue,
    parse_unit   "false" ExpFalse,
    parse_unit   "unit"  ExpUnit,
    parse_single "new"   New,
    parse_single "read"  Read,
    parse_double "write" Write,
    parse_triple "if"    IfElse,
    parse_var            Var,
    parse_abs            Abs,
    parse_app            App
                     ]

