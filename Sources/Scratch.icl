implementation module StdChar

//	Test on Characters:

instance Traversable Maybe where
    traverse _ Nothing = pure Nothing
    traverse f (Just x) = Just <$> f x
    sequenceA f = traverse id f
    mapM f x = unwrapMonad (traverse (WrapMonad o f) x)
    sequence x = mapM id x

instance Traversable [] where
    traverse f x = foldr cons_f (pure []) x
      where cons_f x ys = (\x xs -> [x:xs]) <$> f x <*> ys
    mapM f x = 'CM'.mapM f x
    sequenceA f = traverse id f
    sequence x = mapM id x

instance Traversable (Either a) where
    traverse _ (Left x) = pure (Left x)
    traverse f (Right y) = Right <$> f y
    sequenceA f = traverse id f
    mapM f x = unwrapMonad (traverse (WrapMonad o f) x)
    sequence x = mapM id x

instance Traversable ((,) a) where
    traverse f (x, y) = (\x y -> (x, y)) x <$> f y
    sequenceA f = traverse id f
    mapM f x = unwrapMonad (traverse (WrapMonad o f) x)
    sequence x = mapM id x
*/

infixl 7 *, /
infixl 6 +, -


class Monoid a where
  (+)       :: a -> a -> a
  zero      :: a

  -- Minimal complete definition:
  --   zero and (+)
  -- Instances:
  --   List, Natural
  -- CommutativeMonoid requires extra axiom

class (Monoid a) => Group a where
  (-)       :: a -> a -> a
  negate    :: a -> a

  -- Minimal complete definition:
  --   negate or (-)
  negate x  = zero - x
  x - y     = x + negate y

  -- CommutativeGroup (or AbelianGroup) requires extra axiom

class (Monoid a) => Semiring a where
  (*) :: a -> a -> a
  one :: a

  -- Minimal complete definition:
  --   one and (*)

class (Semiring a, Group a) => Ring a where
  fromInteger :: Integer -> a

  -- Get fromInteger for free!
  fromInteger n
    | n < 0     =  negate (fi (negate n))
    | otherwise =  fi n
    where fi 0  =  zero
          fi 1  =  one
          fi n
            | even n    = fin + fin
            | otherwise = fin + fin + one
            where fin = fi (n `div` 2)

  -- Instances:
  --   Integer
  -- CommutativeRing requires extra axiom

class (Ring a) => Field a where
  recip        :: a -> a
  (/)          :: a -> a -> a
  fromRational :: Rational -> a

  -- Minimal complete definition:
  --   recip or (/)
  recip x        = one / x
  x / y          = x * recip y

  -- Get fromRational for free!
  fromRational x = fromInteger (numerator x) /
                   fromInteger (denominator x)
  -- Instances:
  --   Float, Double, Ratio a, Complex a
  -- Realy a DivisionRing, Field requires commutativity

class (Ring a) => Domain a where
  div, mod  :: a -> a -> a
  divMod    :: a -> a -> (a,a)

  associate :: a -> a
  unit      :: a -> a

  -- Minimal complete definition:
  --   unit and (divMod or (div and mod))
  n `divMod` d = (n `div` d, n `mod` d)
  n `div` d    = q  where (q,r) = divMod n d
  n `mod` d    = r  where (q,r) = divMod n d
  associate x = x `div` unit x

divides :: (Domain a, Eq a) => a -> a -> Bool
divides y x = mod x y == zero

even, odd :: (Domain a, Eq a) => a -> Bool
even n = divides 2 n
odd    = not . even

-- Add Algebraic and Transcendental classes like below.

class (Field a) => Algebraic a where
    sqrt :: a -> a
    root :: Integer -> a -> a
    (^/) :: a -> Rational -> a

    sqrt     = root 2
    root n x = x ^/ (1 % n)
    x ^/ y   = root (denominator y) (x ^- numerator y)

class (Algebraic a) => Transcendental a where
    pi                  :: a
    exp, log            :: a -> a
    logBase, (**)       :: a -> a -> a
    sin, cos, tan       :: a -> a
    asin, acos, atan    :: a -> a
    sinh, cosh, tanh    :: a -> a
    asinh, acosh, atanh :: a -> a

    x ** y           =  exp (log x * y)
    logBase x y      =  log y / log x
    tan  x           =  sin x / cos x
    asin x           =  atan (x / sqrt (1-x^2))
    acos x           =  pi/2 - asin x
    sinh x           =  (exp x - exp (-x)) / 2
    cosh x           =  (exp x + exp (-x)) / 2
    tanh x           =  sinh x / cosh x
    asinh x          =  log (sqrt (x^2+1) + x)
    acosh x          =  log (sqrt (x^2-1) + x)
    atanh x          =  (log (1+x) - log (1-x)) / 2

-- Eq --------------------------------------------------------------------------

-- Eq, Ring => Numerical
--   Not needed, is empty.

-- Eq, Field => Fractional
--  Not needed, is empty

-- Eq, Field => Floating
--  Not needed, is empty

-- Ord -------------------------------------------------------------------------

-- Ord, Ring => Numerical (old Num)
--   Not needed, contains only toRational.
--   Define abs and signum as below.
--   For Complex use maginitude and phase instead of abs and signum.

class (Ord a, Ring a) => Numerical a where

abs :: (Ord a, Group a) => a -> a
abs x = max x (negate x)

signum :: (Ord a, Ring a) => a -> a
signum x | x <  zero = negate one
         | x == zero = zero
         | x >  zero = one

-- Ord, Domain => Integral (old Integral)
--   Not needed, contains only toInteger, define quot and rem as below.

class (Ord a, Domain a) => Integral a where

quot, rem :: (Ord a, Domain a) => a -> a -> (a,a)
n `quot` d = fst $ quotRem n d
n `rem`  d = snd $ quotRem n d

quotRem :: (Ord a, Domain a) => a -> a -> (a,a)
quotRem n d | signum r == -signum d = (q+one, r-d)
            | otherwise             = qr
  where qr@(q,r) = divMod n d

-- Ord, Field => Decimal (Old RealFrac)

class (Ord a, Field a) => Decimal a where
    properFraction   :: (Integral b) => a -> (b,a)
    truncate, round  :: (Integral b) => a -> b
    ceiling, floor   :: (Integral b) => a -> b

    -- Minimal complete definition:
    --      properFraction
    truncate x       =  m  where (m,_) = properFraction x
    round x          =  let (n,r) = properFraction x
                            m     = if r < 0 then n - 1 else n + 1
                          in case signum (abs r - 0.5) of
                                -1 -> n
                                0  -> if even n then n else m
                                1  -> m
    ceiling x        =  if r > 0 then n + 1 else n
                        where (n,r) = properFraction x
    floor x          =  if r < 0 then n - 1 else n
                        where (n,r) = properFraction x

-- Ord, Transcendental => Floating (Old RealFloat)

class (Decimal a, Transcendental a) => Floating a where
    floatRadix       :: a -> Integer
    floatDigits      :: a -> Int
    floatRange       :: a -> (Int,Int)
    decodeFloat      :: a -> (Integer,Int)
    encodeFloat      :: Integer -> Int -> a
    exponent         :: a -> Int
    significand      :: a -> a
    scaleFloat       :: Int -> a -> a
    isNaN, isInfinite, isDenormalized, isNegativeZero, isIEEE
                     :: a -> Bool
    atan2            :: a -> a -> a

    -- Minimal complete definition:
    --      All except exponent, significand, 
    --                 scaleFloat, atan2
    exponent x       =  if m == 0 then 0 else n + floatDigits x
                        where (m,n) = decodeFloat x

    significand x    =  encodeFloat m (- floatDigits x)
                        where (m,_) = decodeFloat x

    scaleFloat k x   =  encodeFloat m (n+k)
                        where (m,n) = decodeFloat x

    atan2 y x
      | x>0           =  atan (y/x)
      | x==0 && y>0   =  pi/2
      | x<0  && y>0   =  pi + atan (y/x) 
      |(x<=0 && y<0)  ||
       (x<0 && isNegativeZero y) ||
       (isNegativeZero x && isNegativeZero y)
                      = -atan2 (-y) x
      | y==0 && (x<0 || isNegativeZero x)
                      =  pi    -- must be after the previous test on zero y
      | x==0 && y==0  =  y     -- must be after the other double zero tests
      | otherwise     =  x + y -- x or y is a NaN, return a NaN (via +)

--------------------------------------------------------------------------------

data Nat
data Natural

data Int
data Integer

data Ratio a = a :/ a
data Rational = Ratio Integer

--data Real
data Float
data Double

data Complex a = a :+ a




instance Eq Real where
(==) x y = prim_eqReal x y

instance Eq (Complex a) where
(==) (Complex x y) (Complex x` y`) = x == x` && y == y`

instance Eq (Maybe a) | Eq a where
(==) Nothing Nothing = True
(==) (Just x) (Just y) = x == y
(==) _ _) = False

instance Eq (Either a b) | Eq a & Eq b where
(==) (Left x) (Left y) = x == y
(==) (Right x) (Right y) = x == y
(==) _ _ = False

instance Eq [a] | Eq a where
(==) [] [] = True
(==) [] _ = False
(==) _ [] = False
(==) [x:xs] [y:ys] = x == y && xs == ys

instance Eq {a} | Eq a where
(==) x y = prim_eqArray x y

instance Eq (Slice a) where
(==) left right
| left.length <> right.length = False
= check left.position right.position
where
lmax = left.position + left.length
// rmax = right.position + right.length
check l r
// | l < lmax && r < rmax = left.string.[l] == right.string.[r] && check (l + 1) (r + 1)
| l < lmax = left.string.[l] == right.string.[r] && check (l + 1) (r + 1)
    = True

    instance Eq (a,b) | Eq a & Eq b
    instance Eq (a,b,c) | Eq a & Eq b & Eq c
    instance Eq (a,b,c,d) | Eq a & Eq b & Eq c & Eq d
    instance Eq (a,b,c,d,e) | Eq a & Eq b & Eq c & Eq d & Eq e

