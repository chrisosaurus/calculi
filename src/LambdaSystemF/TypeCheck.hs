module LambdaSystemF.TypeCheck
(
    typecheck,
    typecheck_with_env,
)
where

import LambdaSystemF.Data.Exp
import Shared.Data.Env

typecheck :: Exp -> Either String Exp
typecheck exp = result
    where blank_env = env_new
          ty        = typecheck_with_env exp blank_env
          result    = case ty of
                           Left str -> Left  str
                           Right (exp', _)  -> Right exp'

typecheck_with_env :: Exp -> Env SystemFType -> Either String (Exp, SystemFType)

-- t var
typecheck_with_env e@(Var name) env =
    case env_read env name of
         Nothing   -> Left $ "Unable to find type of variable '" ++ name ++ "'."
         (Just ty) -> Right (e, ty)

-- t abs
typecheck_with_env (Abs name ty body) env = do
    let new_env = env_write env name ty
    (nbody, body_ty) <- typecheck_with_env body new_env
    let nexpr = Abs name ty nbody
    let ntype = FuncType ty body_ty
    Right $ (nexpr, ntype)

-- t abs closure
-- AbsClosure are created during Eval so should not exist at type checking
-- the absclosure 'env' is of type (Env Exp) and so cannot be used here
typecheck_with_env (AbsClosure _ _ _ _) _ = Left $ "Internal error: typechecker encountered AbsClosure"

-- t app
typecheck_with_env (App left right) env = do
    (el, left_ty)  <- typecheck_with_env left  env
    (er, right_ty) <- typecheck_with_env right env
    case left_ty of
        FuncType from_ty to_ty -> if from_ty == right_ty
                                  then Right ((App el er), to_ty)
                                  else Left $ "Function applied to wrong type. '" ++
                                              (show left_ty) ++
                                              "' was applied to type '" ++
                                              (show right_ty) ++ "'."
        otherwise -> Left $ "Left side of application was of type '" ++
                            (show left_ty) ++
                            "'. Expected function."

-- t Unit
typecheck_with_env ExpUnit  _ = Right (ExpUnit, UnitType)

-- t tru
typecheck_with_env ExpTrue  _ = Right (ExpTrue, BoolType)

-- t fls
typecheck_with_env ExpFalse _ = Right (ExpFalse, BoolType)

-- t ifelse
typecheck_with_env (IfElse cond left right) env = do
    (ec, cond_ty)  <- typecheck_with_env cond  env
    (el, left_ty)  <- typecheck_with_env left  env
    (er, right_ty) <- typecheck_with_env right env
    case cond_ty of
         BoolType  -> if (left_ty == right_ty)
                      then Right ((IfElse ec el er), left_ty)
                      else Left $ "If then branch had type '" ++
                                  (show left_ty) ++
                                  "', but right branch had type '" ++
                                  (show right_ty) ++
                                  "'. Expected them to be of the same type."
         otherwise -> Left $ "If expression condition was of type '" ++
                             (show cond_ty) ++ "'. Expected Bool."

typecheck_with_env (TypeAbs tvar inner) env = do
    (einner, inner_ty) <- typecheck_with_env inner env
    Right $ ((TypeAbs tvar einner), UniversalType tvar inner_ty)

typecheck_with_env o@(TypeApp exp ty) env = do
    (tvar, inner_exp) <- case exp of
        (TypeAbs tvar inner) -> Right (tvar, inner)
        _                    -> Left $ "Type application lhs was not a TypeAbs '" ++ (show exp) ++ "'."
    let nexpr = instantiate_expr inner_exp tvar ty
    typecheck_with_env nexpr env

instantiate_type :: SystemFType -> String -> SystemFType -> SystemFType
instantiate_type UnitType _ _ = UnitType
instantiate_type BoolType _ _ = BoolType
instantiate_type (FuncType left right) tvar ty = FuncType nleft nright
    where nleft  = instantiate_type left  tvar ty
          nright = instantiate_type right tvar ty
instantiate_type (TypeVariable var) tvar ty | var == tvar = ty
instantiate_type t@(TypeVariable _) _    _                = t
-- if we hit a new UniversalType binding the same name, stop traversal (shadowing)
instantiate_type t@(UniversalType var _)      tvar _ | var == tvar = t
instantiate_type (UniversalType var inner_ty) tvar ty              = (UniversalType var (instantiate_type inner_ty tvar ty))

instantiate_expr :: Exp -> String -> SystemFType -> Exp
instantiate_expr (Abs name ty body) tvar tty = Abs name nty nbody
    where nty   = instantiate_type ty   tvar tty
          nbody = instantiate_expr body tvar tty
instantiate_expr (AbsClosure name ty body env) tvar tty = AbsClosure name nty nbody env
    where nty   = instantiate_type ty   tvar tty
          nbody = instantiate_expr body tvar tty
instantiate_expr (App left right) tvar tty = App nleft nright
    where nleft =  instantiate_expr left  tvar tty
          nright = instantiate_expr right tvar tty
instantiate_expr (TypeAbs str body) tvar tty = TypeAbs str nbody
    where nbody = if str == tvar
                  then body
                  else instantiate_expr body tvar tty
instantiate_expr (TypeApp exp ty) tvar tty = TypeApp nexp nty
    where nexp = instantiate_expr exp tvar tty
          nty  = instantiate_type ty  tvar tty
instantiate_expr (IfElse cond left right) tvar tty = IfElse ncond nleft nright
    where ncond  = instantiate_expr cond  tvar tty
          nleft  = instantiate_expr left  tvar tty
          nright = instantiate_expr right tvar tty
instantiate_expr e _ _ = e

