module LambdaSimplyTyped.Data.Exp
(
    Exp (..),
    SimpleType (..),
    is_normal_form,
)
where

import Shared.Data.Env

data SimpleType = UnitType
                | BoolType
                | FuncType SimpleType SimpleType
    deriving (Eq)

instance Show SimpleType where
    show = showSimpleType

showSimpleType :: SimpleType -> String
showSimpleType UnitType = "Unit"
showSimpleType BoolType = "Bool"
showSimpleType (FuncType l r) = res
    where l_t = showSimpleType l
          r_t = showSimpleType r
          res = l_t ++ " -> " ++ r_t

data Exp = Var String
         --   binding type
         | Abs String SimpleType Exp
         -- an Abs is evaluated to an AbsClosure when first encountered
         --          binding type
         | AbsClosure String SimpleType Exp (Env Exp)
         | App Exp Exp
         | ExpUnit
         | ExpTrue
         | ExpFalse
         | IfElse Exp Exp Exp
    deriving (Show, Eq)

is_normal_form :: Exp -> Bool
--is_normal_form (Abs _ _)      = True
is_normal_form (AbsClosure _ _ _ _) = True
is_normal_form ExpUnit        = True
is_normal_form ExpTrue        = True
is_normal_form ExpFalse       = True
is_normal_form _              = False

