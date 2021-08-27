{-# LANGUAGE OverloadedStrings #-}
module LambdaCube.Common.Parser
  ( Parser

  , topParser
  , parenthesized
  , identifier
  , metaIdentifier

  , rightArrow
  , atsignBackslash
  , tripleColon

  , backslash
  , atsign
  , sharp
  , colon
  , semicolon
  , dot
  , openParenthesis
  , closeParenthesis
  , exclamationMark
  , comma
  , asterisk
  , equalsign
  ) where

import           Prelude               hiding (lex)

import           Data.Text             (Text)
import qualified Data.Text             as Text
import           Data.Void             (Void)
import           LambdaCube.Common.Ast
import           Text.Megaparsec
import           Text.Megaparsec.Char

type Parser = Parsec Void Text

topParser :: Parser a -> Parser a
topParser = between space eof

parenthesized :: Parser a -> Parser a
parenthesized = between openParenthesis closeParenthesis

identifier :: Parser Identifier
identifier = lex $ (Text.pack .) . (:) <$> letterChar <*> many (alphaNumChar <|> char '_')

metaIdentifier :: Parser MetaIdentifier
metaIdentifier = lex $ char '$' *> ((:) <$> letterChar <*> many (alphaNumChar <|> char '_'))

rightArrow, atsignBackslash, tripleColon :: Parser Text
rightArrow      = lex $ string "->"
atsignBackslash = lex $ string "@\\"
tripleColon     = lex $ string ":::"

backslash, atsign, sharp, colon, semicolon, dot, openParenthesis, closeParenthesis, exclamationMark, comma, asterisk, equalsign :: Parser Char
backslash        = lex $ char '\\'
atsign           = lex $ char '@'
sharp            = lex $ char '#'
colon            = lex $ char ':'
semicolon        = lex $ char ';'
dot              = lex $ char '.'
openParenthesis  = lex $ char '('
closeParenthesis = lex $ char ')'
exclamationMark  = lex $ char '!'
comma            = lex $ char ','
asterisk         = lex $ char '*'
equalsign        = lex $ char '='

lex :: Parser a -> Parser a
lex = (<* space)
