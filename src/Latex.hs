module Latex where

import Numeric.Natural
import Data.String
import Data.Maybe (fromMaybe)
import Control.Applicative (liftA2)
import Data.Ratio
import qualified Data.Char as Char
import qualified Data.Set as Set; import Data.Set (Set)

type Name = String

-- Subexpressions get precedences so we can tell when to parenPrec.
-- Precedences include a set of tags so that they form a partial order
-- instead of a total one, avoiding unintentional orderings.
-- Nothing = Set of all tags
-- As in Coq, a precedence level of 0 binds tightest
data Precedence = Precedence (Maybe (Set Name)) Natural

-- For convenience
precedence :: [Name] -> Natural -> Precedence
precedence = Precedence . Just . Set.fromList

-- Compare precedences
cmpPrec :: Precedence -> Precedence -> Maybe Ordering
cmpPrec (Precedence a n) (Precedence b m)
  | disjoint a b = Nothing
  | otherwise = Just $ flip compare n m
  where
    disjoint = (fromMaybe False .) . liftA2 Set.disjoint

-- Surround with curly braces if necessary
brac :: String -> String
brac s
  | atomic s = s
  | otherwise = "{" ++ s ++ "}"
  where
    atomic = \case
      [_] -> True
      s@('{' : _) -> last s == '}'
      s -> all Char.isDigit s || all Char.isAlpha s

-- Parenthesize if parent is higher than child or incomparable
parenPrec :: Precedence -> Precedence -> String -> String
parenPrec parent child s
  | parent `gtr` child = brac $ "\\left(" ++ s ++ "\\right)"
  | otherwise = s
  where
    a `gtr` b =
      case a `cmpPrec` b of
        Just GT -> True
        Nothing -> True
        _ -> False

-- One component of a document
data Latex
  = Command Name Document
  | Child Document
  | Paragraph String
  | Verbatim String

-- An entire document
data Document = Document
  { dItems :: [Latex]
  , dPrec :: Precedence
  }

instance Show Document where
  show (Document l p) = concatMap (go p) l where
    go p = \case
      Command f (Document l' q) -> "\\" ++ f ++ " " ++ concatMap (go' q) l'
      Child (Document l' q) -> concatMap (go' q) l'
      Paragraph s -> "\n\n" ++ s ++ "\n\n"
      Verbatim s -> s
      where
        go' q d = brac $ parenPrec p q (go q d)

-- -------------------- Basic documents --------------------

-- Identity function specialized to Document. Slightly less noisy than the :: Document annotation
doc :: Document -> Document
doc = id

-- Mark components with the highest precedence
atoms :: [Latex] -> Document
atoms l = Document l (Precedence Nothing 0)

atomize :: Document -> Document
atomize (Document l p) = atoms l

nested :: Document -> Latex
nested = Child . atomize

-- 'inline LaTeX'
verbatim :: String -> Document
verbatim s = atoms [Verbatim s]

-- An infix operator with the given precedence
operator :: Name -> [Name] -> Natural -> Document -> Document -> Document
operator f tags n l r = Document [nested l, Verbatim f, nested r] (precedence tags n)

-- Surround a document with the given left and right delimiters
wrap :: String -> String -> Document -> Document
wrap l r d = atoms [Verbatim $ "\\left " ++ l, nested d, Verbatim $ "\\right " ++ r]

-- -------------------- Handy instances --------------------

-- Strings for inline LaTeX
instance IsString Document where
  fromString = verbatim

-- Integer literals + arithmetic operators
instance Num Document where
  (+) = operator "+" ["arithmetic"] 50
  (-) = operator "-" ["arithmetic"] 50
  (*) = operator "*" ["arithmetic"] 40
  abs = wrap "|" "|"
  signum = error "not implemented: signum on Documents"
  fromInteger = verbatim . show

-- Rational literals + division
instance Fractional Document where
  l / r = atoms [Command "frac" $ atoms [nested l, nested r]] where
  fromRational r = fromInteger (numerator r) / fromInteger (denominator r)

-- -------------------- Math --------------------

-- Every unicode character must have an equivalent ASCII approximation
-- (\\//), (//\\), (/\), (\/), (∧), (∨)
-- (|^|), (|.|), (\./), (/^\), (∪), (∩)
-- (==>), (===>), (<==), (<===), (<--), (<---), (-->), (--->), (~~>)
-- [] for subscripting

-- -------------------- CS --------------------

-- -------------------- Structures --------------------

-- Lists (enumerated with various bullet styles)
-- Tables
-- Pseudocode
-- Source code
-- Tikz diagrams
-- Graphs
-- Figures (can contain any of the above)
