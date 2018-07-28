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
                           Right _  -> Right exp

typecheck_with_env :: Exp -> Env SimpleType -> Either String SimpleType

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
typecheck_with_env (AbsClosure _ _ _ _) _ = Left $ "Internal error: typechecker encountere AbsClosure"

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

