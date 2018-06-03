module Shared.Data.EnvSpec
(
    main,
    spec,
)
where
import Test.Hspec

import LambdaAssignment.Data.Exp
import Shared.Data.Env

spec :: Spec
spec = do
  describe "testing simple env operations" $ do
    it "write & read" $ do
      let env = env_new
      let key = "some unique key"
      let inserted_val = Var "some really unique string"
      let env' = env_write env key inserted_val
      let fetched_val = env_read env' key
      fetched_val `shouldBe` inserted_val

    it "overwrite" $ do
      let env = env_new
      let key = "some unique key"
      let first_val = Var "some really unique string"
      let second_val = Var "some slightly different but still really unique string"
      let env' = env_write env key first_val
      let env'' = env_write env' key second_val
      let fetched_val = env_read env'' key
      fetched_val `shouldBe` second_val


main :: IO ()
main = hspec spec

