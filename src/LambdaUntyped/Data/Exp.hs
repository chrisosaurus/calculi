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
         | Unit
         | ExpTrue
         | ExpFalse
         | IfElse Exp Exp Exp
    deriving (Show, Eq)

is_normal_form :: Exp -> Bool
--is_normal_form (Abs _ _)      = True
is_normal_form (AbsClosure _ _ _) = True
is_normal_form Unit           = True
is_normal_form ExpTrue        = True
is_normal_form ExpFalse       = True
is_normal_form _              = False

