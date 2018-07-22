module LambdaSimplyTyped.Data.ExpSpec
(
    main,
    spec,
)
where

import Test.Hspec

import LambdaSimplyTyped.Data.Exp
import Shared.Data.Env

spec :: Spec
spec = do
  describe "testing is_normal_form positive cases" $ do
    it "AbsClosure" $ do
      is_normal_form (AbsClosure "string" BoolType ExpUnit env_new) `shouldBe` True
    it "Unit" $ do
      is_normal_form ExpUnit `shouldBe` True
    it "True" $ do
      is_normal_form (ExpTrue) `shouldBe` True
    it "False" $ do
      is_normal_form (ExpFalse) `shouldBe` True

  describe "testing is_normal_form negative cases" $ do
    it "Var" $ do
      is_normal_form (Var "string") `shouldBe` False
    it "App" $ do
      is_normal_form (App ExpUnit ExpUnit) `shouldBe` False
    it "IfElse" $ do
      is_normal_form (IfElse ExpUnit ExpUnit ExpUnit) `shouldBe` False
    it "Abs" $ do
      is_normal_form (Abs "string" BoolType ExpUnit) `shouldBe` False

main :: IO ()
main = hspec spec

