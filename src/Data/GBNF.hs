{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ImportQualifiedPost, LambdaCase #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}

module Data.GBNF where

import Data.List
import Data.Functor
import Control.Applicative
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import qualified Text.Regex.Applicative as RE
import Control.Applicative.Free

--------------------------------------------------------------------------------
-- Core DSL

data Rule a where
  Lit :: String -> Rule ()
  Pattern :: Pattern -> Rule String
  Many :: Ap Rule a -> Rule [a]
  Some :: Ap Rule a -> Rule (NonEmpty a)

manyR :: Ap Rule a -> Ap Rule [a]
manyR = liftAp . Many

someR :: Ap Rule a -> Ap Rule (NonEmpty a)
someR = liftAp . Some

litR :: String -> Ap Rule ()
litR = liftAp . Lit

data Pattern = Pat { classes :: [Class], cardinality :: Cardinality }

data Cardinality = ManyC | SomeC | SingleC

data Class = Range Char Char | Singleton Char | Hyphen
  deriving (Eq)

--------------------------------------------------------------------------------
-- Convert to regex-applicative

patternRegex :: Pattern -> RE.RE Char [Char]
patternRegex (Pat{classes, cardinality}) =
  cardinalityOp $ RE.psym $ \s -> any (($ s) . classPredicate) classes

  where
    classPredicate = \case
      Range a z -> (`elem` [a..z])
      Singleton c -> (== c)
    cardinalityOp = case cardinality of
      ManyC -> many
      SomeC -> some
      SingleC -> fmap (:[])

ruleToRegex :: Rule a -> RE.RE Char a
ruleToRegex = \case
  Lit t -> void $ RE.string t
  Pattern p -> patternRegex p
  Many m -> many $ rulesToRegex m
  Some m -> fmap NE.fromList $ some $ rulesToRegex m

rulesToRegex :: Ap Rule a -> RE.RE Char a
rulesToRegex = runAp ruleToRegex

pat :: Pattern -> Ap Rule String
pat = liftAp . Pattern

manyP :: [Class] -> Ap Rule String
manyP classes = liftAp $ Pattern $ Pat { classes, cardinality = ManyC }

someP :: [Class] -> Ap Rule String
someP classes = liftAp $ Pattern $ Pat { classes, cardinality = SomeC }

singleP :: [Class] -> Ap Rule String
singleP classes = liftAp $ Pattern $ Pat { classes, cardinality = SingleC }

--------------------------------------------------------------------------------
-- Convert to GBNF

rootGBNF :: Ap Rule a -> String
rootGBNF r = "root ::= " ++ rulesGBNF r

rulesGBNF :: Ap Rule a -> String
rulesGBNF = runAp_ ruleGBNF

ruleGBNF :: Rule a -> String
ruleGBNF = \case
  Lit t -> show t ++ " "
  Pattern p -> patternGBNF p ++ " "
  Many m -> "(" ++ rulesGBNF m ++ ")* "
  Some m -> "(" ++ rulesGBNF m ++ ")+ "

patternGBNF :: Pattern -> String
patternGBNF Pat{classes,cardinality} =
  "[" <>
     concatMap classGBNF nonhyphens <>
     concatMap classGBNF hyphens <>
    "]" ++ cardinalityGBNF
   where nonhyphens = filter (/=Hyphen) classes
         hyphens = nub $ filter (==Hyphen) classes
         cardinalityGBNF = case cardinality of
           ManyC -> "*"
           SomeC -> "+"
           SingleC -> ""

classGBNF :: Class -> String
classGBNF = \case
  Singleton c -> [c]
  Hyphen -> "-"
  Range a z -> [a] ++ "-" ++ [z]

--------------------------------------------------------------------------------
-- Demo

rules = pat $ Pat{classes=[Range 'a' 'z'],cardinality=ManyC}

demo =
  RE.match (rulesToRegex rules) "abc"

demo' = rootGBNF rules
