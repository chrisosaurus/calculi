module Shared.LexerSpec
(
    main,
    spec,
)
where

import Test.Hspec

import Shared.Lexer

spec :: Spec
spec = do
  describe "stringify tests" $ do
    it "stringify_tokens 1" $ do
      let expression = [LParen, Lambda, Symbol "x", Period, Symbol "x", RParen]
      let expected = "(\\x.x)"
      stringify_tokens expression `shouldBe` expected
    it "stringify_tokens 2" $ do
      let expression = [LParen, Lambda, Symbol "x", Period, Symbol "x", Symbol "x", Symbol "x", RParen]
      let expected = "(\\x.x x x)"
      stringify_tokens expression `shouldBe` expected

  describe "single lexer tests" $ do
    it "lambda" $ do
      let expression = "\\"
      let expected = Right [Lambda]
      lexer expression `shouldBe` expected
    it "period" $ do
      let expression = "."
      let expected = Right [Period]
      lexer expression `shouldBe` expected
    it "parens" $ do
      let expression = "()"
      let expected = Right [LParen, RParen]
      lexer expression `shouldBe` expected
    it "symbol 1" $ do
      let expression = "x"
      let expected = Right [Symbol "x"]
      lexer expression `shouldBe` expected
    it "symbol 2" $ do
      let expression = "hello"
      let expected = Right [Symbol "hello"]
      lexer expression `shouldBe` expected
    it "symbol 3" $ do
      let expression = "     hello     world   "
      let expected = Right [Symbol "hello", Symbol "world"]
      lexer expression `shouldBe` expected
    it "symbol 4" $ do
      let expression = "abc123"
      let expected = Right [Symbol "abc123"]
      lexer expression `shouldBe` expected
    it "all spaces" $ do
      let expression = "     "
      let expected = Right []
      lexer expression `shouldBe` expected

  describe "compound lexer tests" $ do
    it "simple lambda" $ do
      let expression = "(\\x.x)"
      let expected = Right [LParen, Lambda, Symbol "x", Period, Symbol "x", RParen]
      lexer expression `shouldBe` expected
    it "fixpoint" $ do
      let expression = "(\\x.x x x)"
      let expected = Right [LParen, Lambda, Symbol "x", Period, Symbol "x", Symbol "x", Symbol "x", RParen]
      lexer expression `shouldBe` expected

  describe "lexer error handling" $ do
    it "unknown char" $ do
      let expression = "{"
      let expected = Left "Failed to parse: {"
      lexer expression `shouldBe` expected
    it "spaces" $ do
      let expression = "  {"
      let expected = Left "Failed to parse: {"
      lexer expression `shouldBe` expected
    it "invalid symbol" $ do
      let expression = "he}"
      let expected = Left "Failed to parse: }"
      lexer expression `shouldBe` expected

  describe "Simply Typed" $ do
    it "abstraction 1" $ do
      let expression = "(\\x:Unit.x)"
      let expected = Right [LParen, Lambda, Symbol "x", Colon, Symbol "Unit", Period, Symbol "x", RParen]
      lexer expression `shouldBe` expected
    it "abstraction 2" $ do
      let expression = "((\\id:Bool->Bool.  (if (id true) unit false)) (\\x:Bool.x))"
      let expected = Right [LParen,
                              LParen, Lambda, Symbol "id", Colon, Symbol "Bool", Arrow, Symbol "Bool", Period,
                                LParen, Symbol "if",
                                  LParen, Symbol "id", Symbol "true", RParen,
                                  Symbol "unit", Symbol "false",
                                RParen,
                              RParen,
                              LParen, Lambda, Symbol "x", Colon, Symbol "Bool", Period, Symbol "x", RParen,
                            RParen]
      lexer expression `shouldBe` expected

  describe "System F syntax" $ do
    it "type abstraction" $ do
      let expression = "(/\\X.x)"
      let expected = Right [LParen, LAMBDA, Symbol "X", Period, Symbol "x", RParen]
      lexer expression `shouldBe` expected
    it "type application" $ do
      let expression = "x [Unit]"
      let expected = Right [Symbol "x", LBrack, Symbol "Unit", RBrack]
      lexer expression `shouldBe` expected
    it "universal type" $ do
      let expression = "forall X . x"
      let expected = Right [Symbol "forall", Symbol "X", Period, Symbol "x"]
      lexer expression `shouldBe` expected

  describe "round trip (string -> tokens -> string)" $ do
    it "testing stringifying lexer output gives input" $ do
      mapM_ roundtrip [ "("
                      , ")"
                      , "["
                      , "]"
                      , "\\"
                      , "/\\"
                      , ":"
                      , "."
                      , "->"
                      , "symbol"
                      ]

roundtrip input = expected `shouldBe` result
    where expected = Right input
          temp = lexer input
          result = fmap stringify_tokens temp

main :: IO ()
main = hspec spec

