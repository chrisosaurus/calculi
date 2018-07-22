module LambdaUntyped.Data.Exp
(
    Exp (..),
    is_normal_form,
)
where

import Shared.Data.Env

data Exp = Var String
         | Abs String Exp
         -- an Abs is evaluated to an AbsClosure when first encountered
         | AbsClosure String Exp (Env Exp)
         | App Exp Exp
         | ExpUnit
         | ExpTrue
         | ExpFalse
         | IfElse Exp Exp Exp
    deriving (Eq)

instance Show Exp where
    show = showExp

showExp :: Exp -> String
showExp (Var name) = name
showExp (Abs name body) = "(\\" ++ name ++ "." ++ (showExp body) ++ ")"
showExp (AbsClosure name body _) = showExp (Abs name body)
showExp (App left right) = "(" ++ (showExp left) ++ " " ++
                                  (showExp right) ++ ")"
showExp ExpUnit = "unit"
showExp ExpTrue = "true"
showExp ExpFalse = "false"
showExp (IfElse cond left right) = "(if " ++ (showExp cond)  ++ " " ++
                                             (showExp left)  ++ " " ++
                                             (showExp right) ++ ")"

is_normal_form :: Exp -> Bool
--is_normal_form (Abs _ _)      = True
is_normal_form (AbsClosure _ _ _) = True
is_normal_form ExpUnit           = True
is_normal_form ExpTrue        = True
is_normal_form ExpFalse       = True
is_normal_form _              = False

