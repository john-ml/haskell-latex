module Latex where

import Numeric.Natural
import Data.String
import Data.Maybe (fromMaybe)
import Control.Applicative (liftA2)
import Data.Ratio
import qualified Data.Char as Char
import qualified Data.Set as Set; import Data.Set (Set)
import GHC.Exts
import Debug.Trace

type Name = String

-- Subexpressions get precedences so we can tell when to parenPrec.
-- Precs include a set of tags so that they form a partial order
-- instead of a total one, avoiding unintentional orderings.
-- Nothing = Set of all tags
-- As in Coq, a precedence level of 0 binds tightest
data Prec = Prec (Maybe (Set Name)) Natural deriving Show

-- For convenience
precedence :: [Name] -> Natural -> Prec
precedence = Prec . Just . Set.fromList

-- Compare precedences
cmpPrec :: Prec -> Prec -> Maybe Ordering
cmpPrec (Prec a n) (Prec b m)
  | disjoint a b = Nothing
  | otherwise = Just $ flip compare n m
  where
    disjoint = (fromMaybe False .) . liftA2 Set.disjoint

-- Surround with curly braces if necessary
brac :: String -> String
brac s
  | atomic (strip s) = s
  | otherwise = "{" ++ s ++ "}"
  where
    strip = reverse . shift . reverse . shift where shift = dropWhile Char.isSpace
    atomic = \case
      [_] -> True
      s@('{' : _) -> last s == '}'
      s -> all Char.isDigit s || all Char.isAlpha s

-- Parenthesize if parent is higher than child or incomparable
parenPrec :: Prec -> Prec -> String -> String
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
  = Command Name Doc
  | Child Doc
  | Verbatim String

-- An entire document
data Doc = Doc
  { dItems :: [Latex]
  , dPrec :: Prec
  }

instance Show Doc where
  show (Doc l p) = concatMap (go p) l where
    go p = \case
      Command f (Doc l' q) -> "\\" ++ f ++ " " ++ concatMap (go' q) l'
      Child d@(Doc l' q) -> brac . parenPrec p q $ show d
      Verbatim s -> s
      where
        go' q d = brac $ parenPrec p q (go q d)

-- -------------------- Basic documents --------------------

-- Mark components with the highest precedence
atoms :: [Latex] -> Doc
atoms l = Doc l (Prec Nothing 0)

-- Empty document
empty :: Doc
empty = atoms []

prec :: [Name] -> Natural -> Doc -> Doc
prec tags n (Doc l _) = Doc l (precedence tags n)

atomize :: Doc -> Doc
atomize (Doc l p) = atoms l

-- 'inline LaTeX'
verbatim :: String -> Doc
verbatim s = atoms [Verbatim s]

-- An infix operator with the given precedence
operator :: Name -> [Name] -> Natural -> Doc -> Doc -> Doc
operator f tags n l r =
  Doc
    [Child l, Verbatim $ " " ++ f ++ " ", Child r]
    (precedence tags n)

-- Prefix operator with given precedence
prefix :: Name -> [Name] -> Natural -> Doc -> Doc
prefix f tags n r = Doc [Verbatim f, Child r] (precedence tags n)

-- Postfix operator with given precedence
postfix :: Name -> [Name] -> Natural -> Doc -> Doc
postfix f tags n l = Doc [Child l, Verbatim f] (precedence tags n)

-- Surround a document with the given left and right delimiters
wrap :: String -> String -> Doc -> Doc
wrap l r d = atoms [Verbatim l, Child d, Verbatim r]

-- -------------------- Handy instances --------------------

instance IsString Latex where
  fromString = Verbatim

-- Strings for inline LaTeX
instance IsString Doc where
  fromString = verbatim

-- -- Lists of strings for commands
-- instance IsList Doc where
--   type Item Doc = String
--   fromList = \case
--     (h : t) -> atoms [Command "\\" (atoms $ map fromString t)]
--     _ -> error "fromList :: [String] -> Doc: empty list"
--   toList = error "toList :: [String] -> Doc: not implemented"

-- Integer literals + arithmetic operators
instance Num Doc where
  (+) = operator "+" ["arithmetic"] 50
  (-) = operator "-" ["arithmetic"] 50
  (*) = operator "*" ["arithmetic"] 40
  abs = wrap "\\left|" "\\right|"
  signum = error "not implemented: signum on Docs"
  fromInteger = verbatim . show

-- Rational literals + division
instance Fractional Doc where
  l / r = atoms [Command "frac" $ atoms [Child l, Child r]] where
  fromRational r = fromInteger (numerator r) / fromInteger (denominator r)

-- Placing documents side by side
instance Semigroup Doc where Doc l p <> Doc r q = Doc (l ++ r) p
instance Monoid Doc where mempty = empty

-- -------------------- do notation instances --------------------
-- One for each mode?

-- -------------------- Default mode --------------------

newtype ParaM a = ParaM { doc :: Doc } deriving (Functor)
instance Applicative ParaM where pure = undefined; liftA2 = undefined

-- Concatenate adjacent documents
instance Monad ParaM where
  ParaM l >> ParaM r = ParaM (l <> " " <> r)
  (>>=) = undefined

-- Make paragraph
instance IsString (ParaM a) where fromString = ParaM . verbatim
para :: ParaM a -> ParaM a
para = ParaM . wrap "\n\n" "\n\n" . doc

-- Insert 'inline math'
instance IsList (ParaM a) where
  type Item (ParaM a) = Doc
  fromList = ParaM . wrap "$" "$" . mconcat
  toList = error "toList :: [Doc] -> ParaM a"

-- -------------------- Variable names --------------------

a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z :: IsString a => a
[a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z]
  = map (fromString . pure) (['a'..'z'] :: String)

-- -------------------- Math --------------------
-- Every unicode character must have an equivalent ASCII approximation

-- ---------- Logic ----------

top = prec ["logic", "lattices"] 0 $ atoms [Command "top" empty]
bot = prec ["logic", "lattices"] 0 $ atoms [Command "bot" empty]
neg = prefix "\\lneg" ["logic"] 30

infixl 6 /\
infixl 6 ∧
infixl 5 \/
infixl 5 ∨
(/\) = operator "\\land" ["logic"] 40
(\/) = operator "\\lor" ["logic"] 50
(∧) = (/\)
(∨) = (\/)
-- (\\//), (//\\)

forall :: Doc -> Doc -> Doc
forall x e = Doc ["\\forall ", Child x, ".", Child e] (precedence ["logic"] 90)

exists :: Doc -> Doc -> Doc
exists x e = Doc ["\\exists ", Child x, ".", Child e] (precedence ["logic"] 90)

-- ---------- Arrows ----------

infixl 2 ===>
infixl 2 --->
infixl 2 <===
infixl 2 <---
infixl 2 <==>
infixl 2 <-->
infixl 4 <=>
infixl 4 <->
infixl 4 ==>
infixl 4 -->
infixl 4 <==
infixl 4 <--
a ===> b = (operator "\\Longrightarrow" [] 80 a b)     { dPrec = Prec Nothing 70 }
a ==> b  = (operator "\\Rightarrow" [] 60 a b)         { dPrec = Prec Nothing 70 }
a ---> b = (operator "\\longrightarrow" [] 80 a b)     { dPrec = Prec Nothing 70 }
a --> b  = (operator "\\rightarrow" [] 60 a b)         { dPrec = Prec Nothing 70 }
a <=== b = (operator "\\Longleftarrow" [] 80 a b)      { dPrec = Prec Nothing 70 }
a <== b  = (operator "\\Leftarrow" [] 60 a b)          { dPrec = Prec Nothing 70 }
a <--- b = (operator "\\longleftarrow" [] 80 a b)      { dPrec = Prec Nothing 70 }
a <-- b  = (operator "\\leftarrow" [] 60 a b)          { dPrec = Prec Nothing 70 }
a <==> b = (operator "\\Longleftrightarrow" [] 80 a b) { dPrec = Prec Nothing 70 }
a <=> b  = (operator "\\Leftrightarrow" [] 60 a b)     { dPrec = Prec Nothing 70 }
a <--> b = (operator "\\longleftrightarrow" [] 80 a b) { dPrec = Prec Nothing 70 }
a <-> b  = (operator "\\leftrightarrow" [] 60 a b)     { dPrec = Prec Nothing 70 }

-- ---------- (In)equality ----------

infixl 3 ===
infixl 3 =/=
a === b = (operator "=" [] 70 a b) { dPrec = Prec Nothing 70 }
a =/= b = (operator "\\ne" [] 70 a b) { dPrec = Prec Nothing 70 }

-- ---------- Sets ----------

emptyset = prec ["sets"] 0 $ atoms [Command "emptyset" empty]
ø = emptyset

infixl 4 ∈
within = operator "\\in" ["sets"] 60
(∈) = within

infixl 6 /^\
infixl 6 ∩
infixl 5 \./
infixl 5 ∪
(/^\) = operator "\\cap" ["sets"] 40
(\./) = operator "\\cup" ["sets"] 50
(∩) = (/^\)
(∪) = (\./)

-- Lattices: (|^|), (|.|)

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
