module Shared.Lexer
(
    Token (..),
    lexer,
    stringify_token,
    stringify_tokens,
)
where

import Control.Applicative
import Data.Char (isSpace, isAlphaNum)

data Token = Lambda
           | Period
           | LParen
           | RParen
           | Colon
           | Arrow
           | Symbol String
    deriving (Show, Eq)

stringify_token :: Token -> String
stringify_token Lambda = "\\"
stringify_token Period = "."
stringify_token LParen = "("
stringify_token RParen = ")"
stringify_token Colon  = ":"
stringify_token Arrow  = "->"
stringify_token (Symbol str) = str

stringify_tokens :: [Token] -> String
stringify_tokens [] = ""
stringify_tokens ((Symbol s1):s2@(Symbol _):rest) = (s1) ++ " " ++ stringify_tokens (s2:rest)
stringify_tokens (first:rest) = (stringify_token first) ++  (stringify_tokens rest)

lexer :: String -> Either String [Token]
lexer ""                     = Right []
lexer (ch:rest) | isSpace ch = lexer (trim_space rest)
lexer string                 =
    case     lexer_lambda string
         <|> lexer_period string
         <|> lexer_lparen string
         <|> lexer_rparen string
         <|> lexer_colon  string
         <|> lexer_arrow  string
         <|> lexer_symbol string of
             Nothing                  -> Left $ "Failed to parse: " ++ string
             Just (token, [])         -> Right [token]
             Just (ltoken, remaining) -> case lexer remaining of
                                               Left str -> Left str
                                               Right rtokens -> Right (ltoken:rtokens)

trim_space :: String -> String
trim_space (ch:rest) | isSpace ch = trim_space rest
trim_space str = str

lexer_lambda :: String -> Maybe (Token, String)
lexer_lambda ('\\':rest) = Just (Lambda, rest)
lexer_lambda _           = Nothing

lexer_period :: String -> Maybe(Token, String)
lexer_period ('.':rest) = Just (Period, rest)
lexer_period _          = Nothing

lexer_lparen :: String -> Maybe(Token, String)
lexer_lparen ('(':rest) = Just (LParen, rest)
lexer_lparen _          = Nothing

lexer_rparen :: String -> Maybe(Token, String)
lexer_rparen (')':rest) = Just (RParen, rest)
lexer_rparen _          = Nothing

lexer_colon :: String -> Maybe(Token, String)
lexer_colon (':':rest) = Just (Colon, rest)
lexer_colon _          = Nothing

lexer_arrow :: String -> Maybe(Token, String)
lexer_arrow ('-':'>':rest) = Just (Arrow, rest)
lexer_arrow _              = Nothing

lexer_symbol :: String -> Maybe (Token, String)
lexer_symbol string = lexer_symbol' string ""

--             input     symbol so far
lexer_symbol' :: String -> String -> Maybe (Token, String)
lexer_symbol'   (ch:rest)  ""     | should_discard ch = lexer_symbol' rest ""
lexer_symbol'   (ch:rest)  ""     | isAlphaNum ch     = lexer_symbol' rest [ch]
lexer_symbol'   (ch:rest)  ""                         = Nothing
lexer_symbol'   (ch:rest)  sym    | should_break ch   = Just (Symbol sym, ch:rest)
lexer_symbol'   (ch:rest)  sym    | isAlphaNum ch     = lexer_symbol' rest (sym++[ch])
lexer_symbol'   (ch:rest)  sym                        = Just (Symbol sym, ch:rest)
lexer_symbol'   []         sym                        = Just (Symbol sym, [])

should_discard :: Char -> Bool
should_discard ch | isSpace ch = True
should_discard _               = False

should_break :: Char -> Bool
should_break ch | isSpace ch = True
should_break '('             = True
should_break ')'             = True
should_break '.'             = True
should_break ':'             = True
-- start of arrow
should_break '-'             = True
should_break '\\'            = True
should_break _               = False

