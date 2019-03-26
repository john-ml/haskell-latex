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

-------------------- Basic documents --------------------

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

-------------------- Document templates --------------------

-- Import a bunch of math packages
use :: [Name] -> Doc
use = mconcat . map (\ s -> fromString $ "\\usepackage{" ++ s ++ "}\n")

mathPkgs = ["amsmath", "amsthm", "amsfonts", "setspace"]
graphicsPkgs = ["amsmath", "amsthm", "amsfonts", "setspace", "graphicx"]

-- Standard double-spaced document with class s
document :: String -> Doc -> Doc
document s d =
  fromString ("\\documentclass{" ++ s ++ "}\n")
  <> use (mathPkgs ++ graphicsPkgs)
  <> "\\begin{document}\n"
  <> "\\doublespacing\n"
  <> d
  <> "\\end{document}\n"

article = document "article"
report = document "report"
book = document "book"

-------------------- Handy instances --------------------

instance IsString Latex where
  fromString = Verbatim

-- Strings for inline LaTeX
instance IsString Doc where
  fromString = verbatim

-- Lists of strings for commands
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

-------------------- Modes (for do notation sugar) --------------------

-- Fake monads isomorphic to Doc
class Monad m => Mode m where
  mkMode :: Doc -> m a
  dtMode :: m a -> Doc

-------------------- Default mode --------------------

newtype ParaM a = ParaM { doc :: Doc } deriving (Functor, Semigroup, Monoid, IsString)
instance Applicative ParaM where pure = undefined; liftA2 = undefined

instance Mode ParaM where
  mkMode = ParaM
  dtMode = doc

-- Concatenate adjacent documents
instance Monad ParaM where
  ParaM l >> ParaM r = ParaM (l <> r)
  (>>=) = undefined

-- Make paragraph
para :: Mode m => m a -> m a
para = mkMode . wrap "\n\n" "\n\n" . dtMode

-- Insert 'inline math'
instance IsList (ParaM a) where
  type Item (ParaM a) = Doc
  fromList = ParaM . wrap "$" "$" . mconcat
  toList = error "toList :: [Doc] -> ParaM a"

-------------------- Structures --------------------

---------- Misc. ----------

ref :: Name -> ParaM a
ref s = ParaM . fromString $ "\\ref{" ++ s++ "}"

---------- Headings ----------

heading :: Name -> Name -> ParaM a -> ParaM a
heading kind title body = fromString ("\\" ++ kind ++ "{" ++ title ++ "}\n") <> body

part = heading "part"
part' = heading "part*"
chapter = heading "chapter"
chapter' = heading "chapter*"
section = heading "section"
section' = heading "section*"
subsection = heading "subsection"
subsection' = heading "subsection*"

---------- Figures ----------

figure :: Name -> Name -> ParaM a -> ParaM a
figure label caption body = do
  "\\begin{figure}\n"
  body
  fromString $ "\n\\caption{" ++ caption ++ "}\n"
  fromString $ "\\label{" ++ label ++ "}\n"
  "\\end{figure}\n"

pic :: Name -> ParaM a
pic filename = fromString $ "\\includegraphics[width=\\linewidth]{" ++ filename ++ "}"

chart :: Name -> Name -> Name -> ParaM a
chart label filename caption = figure label caption (pic filename)

---------- Lists ----------
-- (enumerated with various bullet styles)

newtype EnumM a = EnumM { unEnumM :: ParaM a } deriving (Functor, Applicative, Mode, IsString)

instance Monad EnumM where
  EnumM l >> EnumM r = EnumM $ do itemize l; itemize r where
    itemize :: ParaM a -> ParaM a
    itemize = \case
      l@(ParaM (Doc (Verbatim ('\\':'i':'t':'e':'m':_) : _) _)) -> l
      l -> do "\\item\n"; l
  (>>=) = undefined

enumerate :: Mode m => EnumM a -> m a
enumerate (EnumM d) = mkMode . dtMode $ do
  "\\begin{enumerate}\n"
  d
  "\\end{enumerate}\n"

-- Tables
-- Pseudocode
-- Source code
-- Tikz diagrams
-- Graphs

-------------------- Math --------------------
-- Every unicode character must have an equivalent ASCII approximation

---------- Logic ----------

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

---------- Arrows ----------

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

---------- (In)equality ----------

infixl 3 ===
infixl 3 =/=
a === b = (operator "=" [] 70 a b) { dPrec = Prec Nothing 70 }
a =/= b = (operator "\\ne" [] 70 a b) { dPrec = Prec Nothing 70 }

---------- Sets ----------

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

-------------------- CS --------------------

-------------------- Variable names --------------------

a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z :: IsString a => a
[a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z]
  = map (fromString . pure) (['a'..'z'] :: String)

a', b', c', d', e', f', g', h', i', j', k', l', m', n', o',
    p', q', r', s', t', u', v', w', x', y', z' :: IsString a => a
[a', b', c', d', e', f', g', h', i', j', k', l', m', n', o',
    p', q', r', s', t', u', v', w', x', y', z']
  = map (fromString . (: "'")) (['a'..'z'] :: String)

a0, b0, c0, d0, e0, f0, g0, h0, i0, j0, k0, l0, m0, n0, o0,
    p0, q0, r0, s0, t0, u0, v0, w0, x0, y0, z0 :: IsString a => a
[a0, b0, c0, d0, e0, f0, g0, h0, i0, j0, k0, l0, m0, n0, o0,
    p0, q0, r0, s0, t0, u0, v0, w0, x0, y0, z0]
  = map (fromString . ('{' :) . (: "_0}")) (['a'..'z'] :: String)

a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1, o1,
    p1, q1, r1, s1, t1, u1, v1, w1, x1, y1, z1 :: IsString a => a
[a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1, o1,
    p1, q1, r1, s1, t1, u1, v1, w1, x1, y1, z1]
  = map (fromString . ('{' :) . (: "_1}")) (['a'..'z'] :: String)

a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2,
    p2, q2, r2, s2, t2, u2, v2, w2, x2, y2, z2 :: IsString a => a
[a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2,
    p2, q2, r2, s2, t2, u2, v2, w2, x2, y2, z2]
  = map (fromString . ('{' :) . (: "_2}")) (['a'..'z'] :: String)

a3, b3, c3, d3, e3, f3, g3, h3, i3, j3, k3, l3, m3, n3, o3,
    p3, q3, r3, s3, t3, u3, v3, w3, x3, y3, z3 :: IsString a => a
[a3, b3, c3, d3, e3, f3, g3, h3, i3, j3, k3, l3, m3, n3, o3,
    p3, q3, r3, s3, t3, u3, v3, w3, x3, y3, z3]
  = map (fromString . ('{' :) . (: "_3}")) (['a'..'z'] :: String)

a4, b4, c4, d4, e4, f4, g4, h4, i4, j4, k4, l4, m4, n4, o4,
    p4, q4, r4, s4, t4, u4, v4, w4, x4, y4, z4 :: IsString a => a
[a4, b4, c4, d4, e4, f4, g4, h4, i4, j4, k4, l4, m4, n4, o4,
    p4, q4, r4, s4, t4, u4, v4, w4, x4, y4, z4]
  = map (fromString . ('{' :) . (: "_4}")) (['a'..'z'] :: String)

a5, b5, c5, d5, e5, f5, g5, h5, i5, j5, k5, l5, m5, n5, o5,
    p5, q5, r5, s5, t5, u5, v5, w5, x5, y5, z5 :: IsString a => a
[a5, b5, c5, d5, e5, f5, g5, h5, i5, j5, k5, l5, m5, n5, o5,
    p5, q5, r5, s5, t5, u5, v5, w5, x5, y5, z5]
  = map (fromString . ('{' :) . (: "_5}")) (['a'..'z'] :: String)

a6, b6, c6, d6, e6, f6, g6, h6, i6, j6, k6, l6, m6, n6, o6,
    p6, q6, r6, s6, t6, u6, v6, w6, x6, y6, z6 :: IsString a => a
[a6, b6, c6, d6, e6, f6, g6, h6, i6, j6, k6, l6, m6, n6, o6,
    p6, q6, r6, s6, t6, u6, v6, w6, x6, y6, z6]
  = map (fromString . ('{' :) . (: "_6}")) (['a'..'z'] :: String)

a7, b7, c7, d7, e7, f7, g7, h7, i7, j7, k7, l7, m7, n7, o7,
    p7, q7, r7, s7, t7, u7, v7, w7, x7, y7, z7 :: IsString a => a
[a7, b7, c7, d7, e7, f7, g7, h7, i7, j7, k7, l7, m7, n7, o7,
    p7, q7, r7, s7, t7, u7, v7, w7, x7, y7, z7]
  = map (fromString . ('{' :) . (: "_7}")) (['a'..'z'] :: String)

a8, b8, c8, d8, e8, f8, g8, h8, i8, j8, k8, l8, m8, n8, o8,
    p8, q8, r8, s8, t8, u8, v8, w8, x8, y8, z8 :: IsString a => a
[a8, b8, c8, d8, e8, f8, g8, h8, i8, j8, k8, l8, m8, n8, o8,
    p8, q8, r8, s8, t8, u8, v8, w8, x8, y8, z8]
  = map (fromString . ('{' :) . (: "_8}")) (['a'..'z'] :: String)

a9, b9, c9, d9, e9, f9, g9, h9, i9, j9, k9, l9, m9, n9, o9,
    p9, q9, r9, s9, t9, u9, v9, w9, x9, y9, z9 :: IsString a => a
[a9, b9, c9, d9, e9, f9, g9, h9, i9, j9, k9, l9, m9, n9, o9,
    p9, q9, r9, s9, t9, u9, v9, w9, x9, y9, z9]
  = map (fromString . ('{' :) . (: "_9}")) (['a'..'z'] :: String)

ai, bi, ci, di, ei, fi, gi, hi, ii, ji, ki, li, mi, ni, oi,
    pi, qi, ri, si, ti, ui, vi, wi, xi, yi, zi :: IsString a => a
[ai, bi, ci, di, ei, fi, gi, hi, ii, ji, ki, li, mi, ni, oi,
    pi, qi, ri, si, ti, ui, vi, wi, xi, yi, zi]
  = map (fromString . ('{' :) . (: "_i}")) (['a'..'z'] :: String)

aj, bj, cj, dj, ej, fj, gj, hj, ij, jj, kj, lj, mj, nj, oj,
    pj, qj, rj, sj, tj, uj, vj, wj, xj, yj, zj :: IsString a => a
[aj, bj, cj, dj, ej, fj, gj, hj, ij, jj, kj, lj, mj, nj, oj,
    pj, qj, rj, sj, tj, uj, vj, wj, xj, yj, zj]
  = map (fromString . ('{' :) . (: "_j}")) (['a'..'z'] :: String)

ak, bk, ck, dk, ek, fk, gk, hk, ik, jk, kk, lk, mk, nk, ok,
    pk, qk, rk, sk, tk, uk, vk, wk, xk, yk, zk :: IsString a => a
[ak, bk, ck, dk, ek, fk, gk, hk, ik, jk, kk, lk, mk, nk, ok,
    pk, qk, rk, sk, tk, uk, vk, wk, xk, yk, zk]
  = map (fromString . ('{' :) . (: "_k}")) (['a'..'z'] :: String)

an, bn, cn, dn, en, fn, gn, hn, jn, kn, ln, mn, nn, on,
    pn, qn, rn, sn, tn, un, vn, wn, xn, yn, zn :: IsString a => a
[an, bn, cn, dn, en, fn, gn, hn, jn, kn, ln, mn, nn, on,
    pn, qn, rn, sn, tn, un, vn, wn, xn, yn, zn]
  = map (fromString . ('{' :) . (: "_n}")) (['a'..'z'] :: String)

am, bm, cm, dm, em, fm, gm, hm, im, jm, km, lm, mm, nm, om,
    pm, qm, rm, sm, tm, um, vm, wm, xm, ym, zm :: IsString a => a
[am, bm, cm, dm, em, fm, gm, hm, im, jm, km, lm, mm, nm, om,
    pm, qm, rm, sm, tm, um, vm, wm, xm, ym, zm]
  = map (fromString . ('{' :) . (: "_m}")) (['a'..'z'] :: String)

ap, bp, cp, dp, ep, fp, gp, hp, ip, jp, kp, lp, mp, np, op,
    pp, qp, rp, sp, tp, up, vp, wp, xp, yp, zp :: IsString a => a
[ap, bp, cp, dp, ep, fp, gp, hp, ip, jp, kp, lp, mp, np, op,
    pp, qp, rp, sp, tp, up, vp, wp, xp, yp, zp]
  = map (fromString . ('{' :) . (: "_p}")) (['a'..'z'] :: String)

aq, bq, cq, dq, eq, fq, gq, hq, iq, jq, kq, lq, mq, nq, oq,
    pq, qq, rq, sq, tq, uq, vq, wq, xq, yq, zq :: IsString a => a
[aq, bq, cq, dq, eq, fq, gq, hq, iq, jq, kq, lq, mq, nq, oq,
    pq, qq, rq, sq, tq, uq, vq, wq, xq, yq, zq]
  = map (fromString . ('{' :) . (: "_q}")) (['a'..'z'] :: String)
