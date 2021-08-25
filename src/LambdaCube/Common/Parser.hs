{-# LANGUAGE OverloadedStrings #-}
module LambdaCube.Common.Parser
  ( Parser

  , topParser
  , parenthesized
  , identifier

  , rightArrow
  , atsignBackslash

  , backslash
  , atsign
  , sharp
  , colon
  , dot
  , openParenthesis
  , closeParenthesis
  , exclamationMark
  , comma
  , asterisk
  , dollarsign
  ) where

import           Prelude              hiding (lex)

import           Data.Text            (Text)
import qualified Data.Text            as Text
import           Data.Void            (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char

type Parser = Parsec Void Text

topParser :: Parser a -> Parser a
topParser = between space eof

parenthesized :: Parser a -> Parser a
parenthesized = between openParenthesis closeParenthesis

identifier :: Parser Text
identifier = lex $ (Text.pack .) . (:) <$> letterChar <*> many (alphaNumChar <|> char '_')

rightArrow, atsignBackslash :: Parser Text
rightArrow = lex $ string "->"
atsignBackslash = lex $ string "@\\"

backslash, atsign, sharp, colon, dot, openParenthesis, closeParenthesis, exclamationMark, comma, asterisk, dollarsign :: Parser Char
backslash        = lex $ char '\\'
atsign           = lex $ char '@'
sharp            = lex $ char '#'
colon            = lex $ char ':'
dot              = lex $ char '.'
openParenthesis  = lex $ char '('
closeParenthesis = lex $ char ')'
exclamationMark  = lex $ char '!'
comma            = lex $ char ','
asterisk         = lex $ char '*'
dollarsign       = lex $ char '$'

lex :: Parser a -> Parser a
lex = (<* space)
