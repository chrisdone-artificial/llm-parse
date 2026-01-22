{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}

module Data.GBNF where

import Text.ParserCombinators.ReadP
import Control.Applicative.Free
import Control.Alternative.Free

newtype Regex a = Regex { runRegex :: Alt Pattern a }

data Pattern a where
  IntegerS :: Pattern Integer

--------------------------------------------------------------------------------
-- To a parser

-- > (readP_to_S $ regexReadP integerR) "123"
-- [(123,"")]
-- > (readP_to_S $ regexReadP integerR) "abc"
-- []

regexReadP :: Regex a -> ReadP a
regexReadP = runAlt go . runRegex where
  go :: Pattern a -> ReadP a
  go IntegerS = readS_to_P reads

integerR :: Regex Integer
integerR = Regex $ liftAlt IntegerS
