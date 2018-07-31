module LambdaSystemF.Data.Exp
(
    Exp (..),
    SystemFType (..),
    is_normal_form,
)
where

import Shared.Data.Env

data SystemFType = UnitType
                | BoolType
                | FuncType SystemFType SystemFType
                | TypeVariable String
                | UniversalType String SystemFType
    deriving (Eq)

instance Show SystemFType where
    show = showSystemFType

showSystemFType :: SystemFType -> String
showSystemFType UnitType = "Unit"
showSystemFType BoolType = "Bool"
showSystemFType (FuncType l r) = res
    where l_t = showSystemFType l
          r_t = showSystemFType r
          res = l_t ++ "->" ++ r_t
showSystemFType (TypeVariable var) = var
showSystemFType (UniversalType var inner) = res
    where inner_t = showSystemFType inner
          res = "forall " ++ var ++ "." ++ inner_t

data Exp = Var String
         --   binding type
         | Abs String SystemFType Exp
         -- an Abs is evaluated to an AbsClosure when first encountered
         --          binding type
         | AbsClosure String SystemFType Exp (Env Exp)
         | App Exp Exp
         | TypeAbs String Exp
         | TypeApp Exp SystemFType
         | ExpUnit
         | ExpTrue
         | ExpFalse
         | IfElse Exp Exp Exp
    deriving (Eq)

instance Show Exp where
    show = showExp

showExp :: Exp -> String
showExp (Var name) = name
showExp (Abs name ty body) = "(\\" ++ name ++ ":" ++ (show ty) ++ "." ++
                                                     (showExp body) ++ ")"
showExp (AbsClosure name ty body _) = showExp (Abs name ty body)
showExp (App left right) = "(" ++ (showExp left) ++ " " ++
                                  (showExp right) ++ ")"
showExp (TypeAbs name exp) = "(/\\" ++ name ++ "." ++ (showExp exp) ++ ")"
showExp (TypeApp exp ty) = "(" ++ (showExp exp) ++ " [" ++ (show ty) ++ "])"
showExp ExpUnit = "unit"
showExp ExpTrue = "true"
showExp ExpFalse = "false"
showExp (IfElse cond left right) = "(if " ++ (showExp cond)  ++ " " ++
                                             (showExp left)  ++ " " ++
                                             (showExp right) ++ ")"

is_normal_form :: Exp -> Bool
--is_normal_form (Abs _ _)      = True
is_normal_form (AbsClosure _ _ _ _) = True
is_normal_form ExpUnit        = True
is_normal_form ExpTrue        = True
is_normal_form ExpFalse       = True
is_normal_form _              = False

