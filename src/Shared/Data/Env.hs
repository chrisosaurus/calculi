module Shared.Data.Env
(
    Env (..),
    env_new,
    env_read,
    env_write,
)
where

import qualified Data.Map as Map

data Env a = Env (Map.Map String a)
    deriving (Show, Eq)

env_new :: Env a
env_new = Env Map.empty

env_read :: Env a -> String -> a
env_read (Env map) key = case Map.lookup key map of
                            (Just val) -> val
                            Nothing    -> error "env_read: lookup failed"

env_write :: Env a -> String -> a -> Env a
env_write (Env map) key val = Env new_map
    where new_map = Map.insert key val map
