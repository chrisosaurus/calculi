module LambdaAssignment.Data.Context
(
    Context (..)
)
where

import LambdaAssignment.Data.Store
import LambdaAssignment.Data.Exp

data Context = Context Store Env
    deriving (Show, Eq)

