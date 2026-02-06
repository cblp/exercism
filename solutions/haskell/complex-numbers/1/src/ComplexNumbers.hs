module ComplexNumbers (
    Complex,
    conjugate,
    abs,
    exp,
    real,
    imaginary,
    mul,
    add,
    sub,
    div,
    complex,
) where

import Prelude hiding (abs, div, exp)
import Prelude qualified

-- Data definition -------------------------------------------------------------

data Complex a = (:+) {real, imaginary :: a} deriving (Eq, Show)
infix 6 :+

complex :: (a, a) -> Complex a
complex = uncurry (:+)

-- unary operators -------------------------------------------------------------

conjugate :: (Num a) => Complex a -> Complex a
conjugate (a :+ b) = a :+ (-b)

abs :: (Floating a) => Complex a -> a
abs (a :+ b) = sqrt $ sq a + sq b

exp :: (Floating a) => Complex a -> Complex a
exp (a :+ b) = eᵃ * cos b :+ eᵃ * sin b where eᵃ = Prelude.exp a

-- binary operators ------------------------------------------------------------

mul :: (Num a) => Complex a -> Complex a -> Complex a
mul (xᵣ :+ xᵢ) (yᵣ :+ yᵢ) = (xᵣ * yᵣ - xᵢ * yᵢ) :+ (xᵣ * yᵢ + xᵢ * yᵣ)

add :: (Num a) => Complex a -> Complex a -> Complex a
add (xᵣ :+ xᵢ) (yᵣ :+ yᵢ) = (xᵣ + yᵣ) :+ (xᵢ + yᵢ)

sub :: (Num a) => Complex a -> Complex a -> Complex a
sub (xᵣ :+ xᵢ) (yᵣ :+ yᵢ) = (xᵣ - yᵣ) :+ (xᵢ - yᵢ)

div :: (Fractional a) => Complex a -> Complex a -> Complex a
div (xᵣ :+ xᵢ) (yᵣ :+ yᵢ) = (xᵣ * yᵣ + xᵢ * yᵢ) / d :+ (xᵢ * yᵣ - xᵣ * yᵢ) / d
  where
    d = sq yᵣ + sq yᵢ

-- helpers ---------------------------------------------------------------------

sq :: (Num a) => a -> a
sq x = x * x
