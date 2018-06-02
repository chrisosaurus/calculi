module LambdaAssignment.Data.ExpSpec
(
    main,
    spec,
)
where

import Test.Hspec

import LambdaAssignment.Data.Exp

spec :: Spec
spec = do
  describe "testing is_trivial_expression positive cases" $ do
    it "Location" $ do
      is_trivial_expression (Location 0) `shouldBe` True
    it "AbsEnv" $ do
      is_trivial_expression (AbsEnv "string" Unit env_new) `shouldBe` True
    it "Unit" $ do
      is_trivial_expression (Unit) `shouldBe` True
    it "True" $ do
      is_trivial_expression (ExpTrue) `shouldBe` True
    it "False" $ do
      is_trivial_expression (ExpFalse) `shouldBe` True

  describe "testing is_trivial_expression negative cases" $ do
    it "Var" $ do
      is_trivial_expression (Var "string") `shouldBe` False
    it "App" $ do
      is_trivial_expression (App Unit Unit) `shouldBe` False
    it "New" $ do
      is_trivial_expression (New Unit) `shouldBe` False
    it "Read" $ do
      is_trivial_expression (Read Unit) `shouldBe` False
    it "Write" $ do
      is_trivial_expression (Write Unit Unit) `shouldBe` False
    it "IfElse" $ do
      is_trivial_expression (IfElse Unit Unit Unit) `shouldBe` False
    it "Abs" $ do
      is_trivial_expression (Abs "string" Unit) `shouldBe` False

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

