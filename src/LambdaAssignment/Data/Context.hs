module LambdaAssignment.Data.Context
(
    Context (..)
)
where

import LambdaAssignment.Data.Store
import LambdaAssignment.Data.Exp
import Shared.Data.Env

data Context = Context Store (Env Exp)
    deriving (Show, Eq)

