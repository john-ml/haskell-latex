module Latex where

import qualified Data.Set as Set; import Data.Set (Set)

type Name = String

-- Subexpressions get precedences so we can tell when to parenthesize.
-- Precedences include a set of tags so that they form a partial order
-- instead of a total one, avoiding unintentional orderings.
data Precedence = Precedence (Set String) Integer

-- Compare precedences
cmpPrec :: Precedence -> Precedence -> Maybe Ordering
cmpPrec (Precedence a n) (Precedence b m)
  | Set.disjoint a b = Nothing
  | otherwise = Just $ compare n m

-- If parent is higher than child, parenthesize the given string
parenthesize :: Precedence -> Precedence -> String -> String
parenthesize parent child s =
  case parent `cmpPrec` child of
    Just GT -> "\\left(" ++ s ++ "\\right)"
    _ -> s

-- One component of a document
data LatexF a
  = Command Name a
  | Paragraph String
  | Verbatim String
  deriving (Functor, Foldable, Traversable)

-- An entire document
newtype Document = Document
  { unDocument :: [(LatexF Document, Precedence)]
  } deriving (Semigroup, Monoid)

instance Show Document where
  show (Document l) = concatMap (uncurry $ flip go) l where
    go p = \case
      Command s (Document l) -> "\\" ++ s ++ concatMap goArg l
      Paragraph s -> "\n\n" ++ s ++ "\n\n"
      Verbatim s -> s
      where
        goArg (d, q) = "{" ++ parenthesize p q (go q d) ++ "}"
