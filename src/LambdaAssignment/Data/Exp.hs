module LambdaAssignment.Data.Exp
(
    LocationId,
    Exp (..),
    is_normal_form,
)
where

import Shared.Data.Env

type LocationId = Integer

data Exp = Var String
         | Abs String Exp
         -- an Abs is evaluated to an AbsClosure when first encountered
         | AbsClosure String Exp (Env Exp)
         | App Exp Exp
         | Location LocationId
         | New Exp
         | Read Exp
         | Write Exp Exp
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
showExp (New e) = "(new " ++ (showExp e) ++ ")"
showExp (Read e) = "(read " ++ (showExp e) ++ ")"
showExp (Write left right) = "(write " ++ (showExp left)  ++ " " ++
                                          (showExp right) ++ ")"
showExp ExpUnit = "unit"
showExp ExpTrue = "true"
showExp ExpFalse = "false"
showExp (IfElse cond left right) = "(if " ++ (showExp cond)  ++ " " ++
                                             (showExp left)  ++ " " ++
                                             (showExp right) ++ ")"

is_normal_form :: Exp -> Bool
is_normal_form (Location _)   = True
--is_normal_form (Abs _ _)      = True
is_normal_form (AbsClosure _ _ _) = True
is_normal_form ExpUnit           = True
is_normal_form ExpTrue        = True
is_normal_form ExpFalse       = True
is_normal_form _              = False

