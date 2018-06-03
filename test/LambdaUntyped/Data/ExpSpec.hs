module LambdaUntyped.Data.ExpSpec
(
    main,
    spec,
)
where

import Test.Hspec

import LambdaUntyped.Data.Exp
import Shared.Data.Env

spec :: Spec
spec = do
  describe "testing is_normal_form positive cases" $ do
    it "AbsClosure" $ do
      is_normal_form (AbsClosure "string" Unit env_new) `shouldBe` True
    it "Unit" $ do
      is_normal_form (Unit) `shouldBe` True
    it "True" $ do
      is_normal_form (ExpTrue) `shouldBe` True
    it "False" $ do
      is_normal_form (ExpFalse) `shouldBe` True

  describe "testing is_normal_form negative cases" $ do
    it "Var" $ do
      is_normal_form (Var "string") `shouldBe` False
    it "App" $ do
      is_normal_form (App Unit Unit) `shouldBe` False
    it "IfElse" $ do
      is_normal_form (IfElse Unit Unit Unit) `shouldBe` False
    it "Abs" $ do
      is_normal_form (Abs "string" Unit) `shouldBe` False

main :: IO ()
main = hspec spec

