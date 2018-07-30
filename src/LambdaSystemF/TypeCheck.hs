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
-- TODO FIXME the problem here is we *throw* away the new types, which have been instantiated,
-- and instead return the original expression in all it's uninstantiated glory
                           Right _  -> Right exp

typecheck_with_env :: Exp -> Env SystemFType -> Either String SystemFType

-- t var
typecheck_with_env (Var name) env =
    case env_read env name of
         Nothing   -> Left $ "Unable to find type of variable '" ++ name ++ "'."
         (Just ty) -> Right ty

-- t abs
typecheck_with_env (Abs name ty body) env = do
    let new_env = env_write env name ty
    body_ty <- typecheck_with_env body new_env
    Right $ FuncType ty body_ty

-- t abs closure
-- AbsClosure are created during Eval so should not exist at type checking
-- the absclosure 'env' is of type (Env Exp) and so cannot be used here
typecheck_with_env (AbsClosure _ _ _ _) _ = Left $ "Internal error: typechecker encountered AbsClosure"

-- t app
typecheck_with_env (App left right) env = do
    left_ty  <- typecheck_with_env left  env
    right_ty <- typecheck_with_env right env
    case left_ty of
        FuncType from_ty to_ty -> if from_ty == right_ty
                                  then Right to_ty
                                  else Left $ "Function applied to wrong type. '" ++
                                              (show left_ty) ++
                                              "' was applied to type '" ++
                                              (show right_ty) ++ "'."
        otherwise -> Left $ "Left side of application was of type '" ++
                            (show left_ty) ++
                            "'. Expected function."

-- t Unit
typecheck_with_env ExpUnit  _ = Right UnitType

-- t tru
typecheck_with_env ExpTrue  _ = Right BoolType

-- t fls
typecheck_with_env ExpFalse _ = Right BoolType

-- t ifelse
typecheck_with_env (IfElse cond left right) env = do
    cond_ty  <- typecheck_with_env cond  env
    left_ty  <- typecheck_with_env left  env
    right_ty <- typecheck_with_env right env
    case cond_ty of
         BoolType  -> if (left_ty == right_ty)
                      then Right left_ty
                      else Left $ "If then branch had type '" ++
                                  (show left_ty) ++
                                  "', but right branch had type '" ++
                                  (show right_ty) ++
                                  "'. Expected them to be of the same type."
         otherwise -> Left $ "If expression condition was of type '" ++
                             (show cond_ty) ++ "'. Expected Bool."

typecheck_with_env (TypeAbs tvar inner) env = do
    inner_ty <- typecheck_with_env inner env
    Right $ UniversalType tvar inner_ty

typecheck_with_env (TypeApp exp ty) env = do
    exp_ty <- typecheck_with_env exp env
    case exp_ty of
        (UniversalType tvar body) -> Right $ instantiate_type body tvar ty
        ty                        -> Left $ "Type application had lhs with type '" ++
                                            (show exp_ty) ++ "', expected a universal type"

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
instantiate_type (UniversalType var inner_ty) tvar ty              = instantiate_type inner_ty tvar ty

