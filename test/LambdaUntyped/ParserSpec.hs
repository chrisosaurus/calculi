module LambdaUntyped.ParserSpec
(
    main,
    spec,
)
where

import Test.Hspec

import Shared.Lexer
import LambdaUntyped.Parser
import LambdaUntyped.Data.Exp

spec :: Spec
spec = do
  describe "simple parser tests" $ do
    it "Unit" $ do
      let expression = [Symbol "unit"]
      let expected = Right ExpUnit
      parse expression `shouldBe` expected
    it "True" $ do
      let expression = [Symbol "true"]
      let expected = Right ExpTrue
      parse expression `shouldBe` expected
    it "False" $ do
      let expression = [Symbol "false"]
      let expected = Right ExpFalse
      parse expression `shouldBe` expected
    it "Var" $ do
      let expression = [Symbol "x"]
      let expected = Right (Var "x")
      parse expression `shouldBe` expected
    it "If" $ do
      let expression = [LParen, Symbol "if", Symbol "x", Symbol "y", Symbol "z", RParen]
      let expected = Right (IfElse (Var "x") (Var "y") (Var "z"))
      parse expression `shouldBe` expected
    it "app" $ do
      let expression = [LParen, Symbol "x", Symbol "y", RParen]
      let expected = Right (App (Var "x") (Var "y"))
      parse expression `shouldBe` expected
    it "abs" $ do
      let expression = [LParen, Lambda, Symbol "x", Period, Symbol "x", RParen]
      let expected = Right (Abs "x" (Var "x"))
      parse expression `shouldBe` expected

  describe "compound parser tests" $ do
    it "fixpoint" $ do
      let expression = [LParen, Lambda, Symbol "x", Period, LParen, Symbol "x", Symbol "x", RParen, RParen]
      let expected = Right (Abs "x" (App (Var "x") (Var "x")))
      parse expression `shouldBe` expected

main :: IO ()
main = hspec spec

