module LambdaAssignment.Data.Exp
(
    LocationId,
    Exp (..),
    is_normal_form,
    EnvVal,
    Env (..),
    env_new,
    env_read,
    env_write,
)
where

import qualified Data.Map as Map

type LocationId = Integer

data Exp = Var String
         | Abs String Exp
         -- an Abs is evaluated to an AbsClosure when first encountered
         | AbsClosure String Exp Env
         | App Exp Exp
         | Unit
         | Location LocationId
         | New Exp
         | Read Exp
         | Write Exp Exp
         | ExpTrue
         | ExpFalse
         | IfElse Exp Exp Exp
    deriving (Show, Eq)

is_normal_form :: Exp -> Bool
is_normal_form (Location _)   = True
--is_normal_form (Abs _ _)      = True
is_normal_form (AbsClosure _ _ _) = True
is_normal_form Unit           = True
is_normal_form ExpTrue        = True
is_normal_form ExpFalse       = True
is_normal_form _              = False

type EnvVal = Exp
data Env = Env (Map.Map String EnvVal)
    deriving (Show, Eq)

env_new :: Env
env_new = Env Map.empty

env_read :: Env -> String -> EnvVal
env_read (Env map) key = case Map.lookup key map of
                            (Just val) -> val
                            Nothing    -> error "env_read: lookup failed"

env_write :: Env -> String -> EnvVal -> Env
env_write (Env map) key val = Env new_map
    where new_map = Map.insert key val map
