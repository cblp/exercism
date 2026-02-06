module RationalNumbers (
    Rational,
    abs,
    numerator,
    denominator,
    add,
    sub,
    mul,
    div,
    pow,
    expRational,
    expReal,
    rational,
) where

import Prelude hiding (Rational, abs, div, fromRational)
import Prelude qualified

-- Data definition -------------------------------------------------------------

data Rational a = (:/) {numerator, denominator :: a} deriving (Eq, Show)

rational :: (Integral a) => (a, a) -> Rational a
rational (n, d) =
    case compare d 0 of
        LT -> reduce (-n) (-d) -- TODO
        EQ -> undefined
        GT -> reduce n d

reduce :: (Integral a) => a -> a -> Rational a
reduce a b = (a `quot` g) :/ (b `quot` g) where g = gcd a b

fromRational :: (Fractional b, Integral a) => Rational a -> b
fromRational (n :/ d) = fromIntegral n / fromIntegral d

-- unary operators -------------------------------------------------------------

abs :: (Integral a) => Rational a -> Rational a
abs (n :/ d) = Prelude.abs n :/ d

-- binary operators ------------------------------------------------------------

add :: (Integral a) => Rational a -> Rational a -> Rational a
add (an :/ ad) (bn :/ bd) = reduce (an * bd + bn * ad) (ad * bd)

sub :: (Integral a) => Rational a -> Rational a -> Rational a
sub (an :/ ad) (bn :/ bd) = reduce (an * bd - bn * ad) (ad * bd)

mul :: (Integral a) => Rational a -> Rational a -> Rational a
mul (an :/ ad) (bn :/ bd) = reduce (an * bn) (ad * bd)

div :: (Integral a) => Rational a -> Rational a -> Rational a
div (an :/ ad) (bn :/ bd) = rational (an * bd, ad * bn)

pow :: (Integral a) => Rational a -> a -> Rational a
pow (n :/ d) p
    | p >= 0 = (n ^ p) :/ (d ^ p)
    | n < 0 = ((-d) ^ (-p)) :/ ((-n) ^ (-p))
    | otherwise = (d ^ (-p)) :/ (n ^ (-p))

expRational :: (Integral a, Floating b) => Rational a -> b -> b
expRational a p = fromRational a ** p

expReal :: (Floating a, Integral b) => a -> Rational b -> a
expReal a p = a ** fromRational p
