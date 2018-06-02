module LambdaAssignment.Eval
(
    eval,
)
where

import LambdaAssignment.Data.Exp
import LambdaAssignment.Data.Store
import LambdaAssignment.Data.Context

eval_with_context :: Exp -> Context -> (Exp, Context)

eval_with_context v context | is_trivial_expression v = (v, context)


eval_with_context (New exp) context@(Context store env) = ((Location key), (Context store'' env'))
    where (val, (Context store' env')) = eval_with_context exp context
          (key, store'') = store_alloc store val

eval_with_context (Write exp1 exp2) context = (Unit, (Context new_store env))
    where ((Location key), context1)            = eval_with_context exp1 context
          (val,            (Context store env)) = eval_with_context exp2 context1
          new_store                             = store_write store key val

eval_with_context (Read (Location key)) context@(Context store _) = (val, context)
    where val = store_read store key

eval_with_context (Read exp1) context@(Context _ _) = eval_with_context (Read loc) new_context
    where (loc, new_context) = eval_with_context exp1 context


eval_with_context (IfElse exp1 exp2 exp3) context = case cond of
                                                    ExpTrue  -> eval_with_context exp2 context1
                                                    ExpFalse -> eval_with_context exp3 context1
    where (cond, context1) = eval_with_context exp1 context

eval_with_context (Var string) context@(Context _ env) = (val, context)
    where val = env_read env string

-- NB: eval App AbsEnv returns original context, the new_context is only used within body of app (exp2)
eval_with_context (App (AbsEnv string body absenv) exp2) context@(Context store _) = (result, context)
    where (arg, (Context new_store _)) = eval_with_context exp2 context
          new_env = env_write absenv string arg
          new_context = Context new_store new_env
          (result, garbage_context) = eval_with_context body new_context

eval_with_context (App exp1 exp2) context = eval_with_context new_app new_context
    where (first, new_context) = eval_with_context exp1 context
          new_app = App first exp2

eval_with_context (Abs string exp) context@(Context _ env) = (new_abs, context)
    where new_abs = AbsEnv string exp env

eval :: Exp -> Exp
eval exp = result
    where blank_store = store_new
          blank_env   = env_new
          blank_context = Context blank_store blank_env
          (result, context) = eval_with_context exp blank_context

