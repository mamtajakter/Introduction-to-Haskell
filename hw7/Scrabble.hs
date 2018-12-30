{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}

module Scrabble where

import Data.Char

newtype Score = Score Int
  deriving (Eq, Ord, Show, Num)

instance Monoid Score where
  mempty  = Score 0
  mappend = (+)


instance Semigroup Score where
  (Score x) <> (Score x') = Score (x + x')

score ::Char -> Score
score c
    | elem (toLower c) "aeilnorstu" = Score  1
    | elem (toLower c) "dg"         = Score  2
    | elem (toLower c) "bcmp"       = Score  3
    | elem (toLower c) "fhvwy"      = Score  4
    | elem (toLower c) "k"          = Score  5
    | elem (toLower c) "jx"         = Score  8
    | elem (toLower c) "qz"         = Score 10
    | otherwise                     = Score  0

scoreString :: String-> Score
scoreString s= mconcat (map score s)


getScore :: Score-> Int
getScore (Score x)= x
