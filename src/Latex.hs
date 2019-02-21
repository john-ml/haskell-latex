module Latex where

import Numeric.Natural
import Data.String
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
    disjoint Nothing _ = False
    disjoint _ Nothing = False
    disjoint (Just a) (Just b) = Set.disjoint a b

-- Parenthesize if parent is higher than child or incomparable
parenPrec :: Precedence -> Precedence -> String -> String
parenPrec parent child s =
  case parent `cmpPrec` child of
    Just GT -> "{\\left(" ++ s ++ "\\right)}"
    Nothing -> "{\\left(" ++ s ++ "\\right)}"
    _ -> s

-- Surround with curly braces if necessary
brac :: String -> String
brac s
  | atomic s = s
  | otherwise = "{" ++ s ++ "}"
  where
    (f ||| g) x = f x || g x
    atomic = all Char.isDigit ||| all Char.isAlpha

-- One component of a document
data LatexF a
  = Command Name a
  | Child a
  | Paragraph String
  | Verbatim String

-- An entire document
data Document = Document
  { dItems :: [LatexF Document]
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

-- Mark components with the highest precedence
atoms :: [LatexF Document] -> Document
atoms l = Document l (Precedence Nothing 0)

-- 'inline LaTeX'
verbatim :: String -> Document
verbatim s = atoms [Verbatim s]

-- An infix operator with the given precedence
operator :: Name -> [Name] -> Natural -> Document -> Document -> Document
operator f tags n l r = Document [Child l, Verbatim f, Child r] (precedence tags n)

-- Surround a document with the given left and right delimiters
wrap :: String -> String -> Document -> Document
wrap l r d = atoms [Verbatim $ "\\left " ++ l, Child d, Verbatim $ "\\right " ++ r]

-- -------------------- Handy instances --------------------

instance IsString Document where
  fromString = verbatim

instance Num Document where
  (+) = operator "+" ["arithmetic"] 50
  (-) = operator "-" ["arithmetic"] 50
  (*) = operator "*" ["arithmetic"] 40
  abs = wrap "|" "|"
  signum = error "not implemented: signum on Documents"
  fromInteger = verbatim . show

instance Fractional Document where
  l / r = atoms [Command "frac" $ atoms [Child l, Child r]]
