module LambdaAssignment.Data.StoreSpec
(
    main,
    spec,
)
where

import Test.Hspec

import LambdaAssignment.Data.Exp
import LambdaAssignment.Data.Store
import qualified Data.Map as Map

spec :: Spec
spec = do
  describe "testing simple store operations" $ do
    it "alloc" $ do
      let store = store_new
      let (loc_1, store') = store_alloc store ExpTrue
      let (loc_2, store'') = store_alloc store' ExpTrue
      loc_1 `shouldBe` 0
      loc_2 `shouldBe` 1

    it "write & read" $ do
      let store = store_new
      let (key, store') = store_alloc store ExpTrue
      let inserted_val = Var "some really unique string"
      let store'' = store_write store' key inserted_val
      let fetched_val = store_read store'' key
      fetched_val `shouldBe` inserted_val
      key `shouldBe` 0

    it "overwrite" $ do
      let store = store_new
      let (key, store') = store_alloc store ExpTrue
      let first_val = Var "some really unique string"
      let second_val = Var "some slightly different but still really unique string"
      let store'' = store_write store' key first_val
      let store''' = store_write store'' key second_val
      let fetched_val = store_read store''' key
      fetched_val `shouldBe` second_val
      key `shouldBe` 0


main :: IO ()
main = hspec spec

