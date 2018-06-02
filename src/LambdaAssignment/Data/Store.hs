module LambdaAssignment.Data.Store
(
    StoreVal,
    Store (..),
    store_alloc,
    store_new,
    store_read,
    store_write,
)
where

import qualified Data.Map as Map
import LambdaAssignment.Data.Exp

type StoreVal = Exp
--                 vv next available id
data Store = Store LocationId (Map.Map LocationId StoreVal)
    deriving (Show, Eq)

store_new :: Store
store_new = Store 0 Map.empty

store_alloc :: Store -> StoreVal -> (LocationId, Store)
store_alloc (Store loc_id map) val = (loc_id, (Store new_id new_map))
    where new_id = loc_id + 1
          new_map = Map.insert loc_id val map

store_read :: Store -> LocationId -> StoreVal
store_read (Store _ map) key = case Map.lookup key map of
                                (Just val) -> val
                                Nothing    -> error "store_read: lookup failed"

store_write :: Store -> LocationId -> StoreVal -> Store
store_write (Store loc_id map) key val = (Store loc_id new_map)
    where new_map = Map.insert key val map

