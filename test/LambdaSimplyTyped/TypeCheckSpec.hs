module LambdaSimplyTyped.TypeCheckSpec
(
    main,
    spec,
)
where

import Test.Hspec

import LambdaSimplyTyped.Data.Exp
import LambdaSimplyTyped.TypeCheck
import Shared.Data.Env

spec :: Spec
spec = do
  let blank_env = env_new
  describe "simple typecheck tests" $ do
    it "Unit" $ do
      let expression = ExpUnit
      let expected = Right UnitType
      typecheck_with_env expression blank_env `shouldBe` expected

    it "True" $ do
      let expression = ExpTrue
      let expected = Right BoolType
      typecheck_with_env expression blank_env `shouldBe` expected

    it "False" $ do
      let expression = ExpFalse
      let expected = Right BoolType
      typecheck_with_env expression blank_env `shouldBe` expected

    it "IfElse True error" $ do
      let expression = IfElse ExpTrue ExpUnit ExpUnit
      let expected = Right UnitType
      typecheck_with_env expression blank_env `shouldBe` expected

  describe "simple typecheck error tests" $ do
    it "IfElse Unit condition error" $ do
      let expression = IfElse ExpUnit ExpUnit ExpUnit
      let expected = Left "If expression condition was of type 'Unit'. Expected Bool."
      typecheck_with_env expression blank_env `shouldBe` expected

    it "IfElse True error" $ do
      let expression = IfElse ExpTrue ExpUnit (Var "non-existant")
      let expected = Left "Unable to find type of variable 'non-existant'."
      typecheck_with_env expression blank_env `shouldBe` expected

    it "IfElse False error" $ do
      let expression = IfElse ExpFalse (Var "non-existant") ExpUnit
      let expected = Left "Unable to find type of variable 'non-existant'."
      typecheck_with_env expression blank_env `shouldBe` expected

  describe "lambda tests" $ do
    it "simple lambda" $ do
      let expression = (Abs "x" BoolType (Var "x"))
      let expected = Right $ FuncType BoolType BoolType
      typecheck_with_env expression blank_env `shouldBe` expected

    it "simple application" $ do
      let expression = (App (Abs "x" BoolType (Var "x")) ExpTrue)
      let expected = Right BoolType
      typecheck_with_env expression blank_env `shouldBe` expected

  describe "lambda error tests" $ do
    it "incorrect application" $ do
      let expression = (App (Abs "x" BoolType (Var "x")) ExpUnit)
      let expected = Left "Function applied to wrong type. 'Bool->Bool' was applied to type 'Unit'."
      typecheck_with_env expression blank_env `shouldBe` expected

main :: IO ()
main = hspec spec

