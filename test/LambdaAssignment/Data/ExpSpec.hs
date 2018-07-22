module LambdaAssignment.Data.ExpSpec
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
  describe "testing is_normal_form positive cases" $ do
    it "Location" $ do
      is_normal_form (Location 0) `shouldBe` True
    it "AbsClosure" $ do
      is_normal_form (AbsClosure "string" ExpUnit env_new) `shouldBe` True
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
    it "New" $ do
      is_normal_form (New ExpUnit) `shouldBe` False
    it "Read" $ do
      is_normal_form (Read ExpUnit) `shouldBe` False
    it "Write" $ do
      is_normal_form (Write ExpUnit ExpUnit) `shouldBe` False
    it "IfElse" $ do
      is_normal_form (IfElse ExpUnit ExpUnit ExpUnit) `shouldBe` False
    it "Abs" $ do
      is_normal_form (Abs "string" ExpUnit) `shouldBe` False

main :: IO ()
main = hspec spec

