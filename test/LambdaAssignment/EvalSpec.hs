module LambdaAssignment.EvalSpec
(
    main,
    spec,
)
where

import Test.Hspec

import LambdaAssignment.Data.Exp
import LambdaAssignment.Eval
import Shared.Data.Env

spec :: Spec
spec = do
  describe "simple eval tests" $ do
    it "Unit" $ do
      let expression = ExpUnit
      let expected = ExpUnit
      eval expression `shouldBe` expected

    it "Location" $ do
      let expression = (Location 0)
      let expected = Location 0
      eval expression `shouldBe` expected

    it "True" $ do
      let expression = ExpTrue
      let expected = ExpTrue
      eval expression `shouldBe` expected

    it "False" $ do
      let expression = ExpFalse
      let expected = ExpFalse
      eval expression `shouldBe` expected

    it "IfElse True" $ do
      let expression = IfElse ExpTrue (Location 0) (Var "doesnt exist, will blow up if eval-ed")
      let expected = Location 0
      eval expression `shouldBe` expected

    it "IfElse False" $ do
      let expression = IfElse ExpFalse (Var "doesnt exist, will blow up if eval-ed") (Location 1)
      let expected = Location 1
      eval expression `shouldBe` expected

  describe "lambda tests" $ do
    it "simple lambda" $ do
      let expression = (Abs "x" (Var "x"))
      let expected = (AbsClosure "x" (Var "x") env_new)
      eval expression `shouldBe` expected

    it "simple application" $ do
      let expression = (App (Abs "x" (Var "x")) (Location 1))
      let expected = Location 1
      eval expression `shouldBe` expected

  describe "state tests" $ do
    it "new" $ do
      let expression = New ExpTrue
      let expected = Location 0
      eval expression `shouldBe` expected

    it "new & read" $ do
      let expression = Read (New ExpTrue)
      let expected = ExpTrue
      eval expression `shouldBe` expected

    it "new & write" $ do
      let expression = (App
                            (Abs "x" (App
                                        (Abs "ignored" (Read (Var "x")))
                                        (Write (Var "x") ExpTrue)))
                            (New ExpFalse))
      let expected = ExpTrue
      eval expression `shouldBe` expected

    it "application side effects" $ do
      let expression = (App
                         (Abs "seq" (App
                                      (Abs "x" (App
                                                 (App (Var "seq") (Write (Var "x") ExpTrue))
                                                 (Read (Var "x"))))
                                      (New ExpFalse)))
                         (Abs "ignore" (Abs "a" (Var "a"))))
      let expected = ExpTrue
      eval expression `shouldBe` expected

main :: IO ()
main = hspec spec

