
/// ## Types

module Data.Nat

:: Nat = 0 | 1 | 2 | ...

module Data.Int

:: Int = ... | -1 | 0 | 1 | ...

module Data.Ratio

:: Ratio = Ratio Int Nat

module Data.Real

:: Real = ... | 0 | ...

module Data.Maybe

:: Maybe a = Just a | Nothing

module Data.Either

:: Either a b = Left a | Right b

module Data.Tuple

:: (a,b)
:: (a,b,c)
:: (a,b,c,d)
:: (a,b,c,d,e)
...

module Data.List

:: [a] = [] | a : [a]

module Data.Array

:: {a}

module Data.String

:: String :== {#Char}

module Data.Slice

:: Slice a = Slice {a} !Int !Int

module Data.Vector

:: Vector a :== {a}

module Data.Matrix

:: Matrix a :== {{a}}

/// # Basics

module Data.Eq

class Eq where
    (==) :: a a -> Bool
    (/=) :: a a -> Bool

module Data.Ord

:: Ordering = Lesser | Equal | Greater

class Ord where
    compare :: a a -> Ordering
    (<) :: a a -> Bool
    (<=) :: a a -> Bool
    (>=) :: a a -> Bool
    (>) :: a a -> Bool

module Data.Bounded

class UpperBounded a | Ord a where
    top :: a

class LowerBounded a | Ord a where
    bottom :: a

class Bounded a | UpperBounded, LowerBounded a

instance Bounded Nat
instance Bounded Int
instance Bounded (Complex a) | Bounded a

module Data.Enum

class Enum a where
    fromEnum :: a -> Int
    toEnum :: Int -> a

    enumFrom :: a -> [a]
    enumFromTo :: a a -> [a]
    enumFromThen :: a a -> [a]
    enumFromThenTo :: a a a -> [a]

/// # Show and Parse

module Data.Show

class Show a where
    show :: a -> String

module Data.Parse

class Parse a where
    parse :: String -> Maybe a

/// # Algebra

module Control.Monoid

class Semigroup a where
    (+) infixl 6 :: a a -> a

// Could be: Add
class Monoid a | Semigroup a where
    neutral :: a


module Control.Algebra

// Could be: Sub
class Group a | Monoid a where
    (-) infixl 6 :: a a -> a
    inverse :: a -> a

// Could be: Mul
class Semiring a | Monoid a where
    (*) infixl 7 :: a a -> a
    unity :: a

// Could be: Num
class Ring a | Group, Semiring a

class Domain a | Ring a where
    (div) infix 7 :: a a -> a
    (mod) infix 7 :: a a -> a

class Field a | Ring a where
    (/) infixl 7 :: a a -> a
    reciprocal :: a -> a

class Module a v | Ring a & Group v where
    (.*) infixl 5 :: a v -> v
    (.*.) infixr 2 :: v v -> a

(*.) infixl 5 :== flip (.*)

// Could be: Root
class Algebraic a | Field a where
    root :: a -> a
    sqrt :: a -> a

// Could be: Trig
class Transcedental a | Algebraic a where
    ln :: a  -> a
    log10 :: a  -> a
    exp :: a  -> a
    
    sin :: a -> a
    cos :: a -> a
    tan :: a -> a
    asin :: a -> a
    acos :: a -> a
    atan :: a -> a
    sinh :: a -> a
    cosh :: a -> a
    tanh :: a -> a
    asinh :: a -> a
    acosh :: a -> a
    atanh :: a -> a

module Data.Signed

// LAWS:
//   
//   signum a * abs a == a
//
// OR Sign?
class Signed a | Ord, Ring a where
    signum :: a -> a
    abs :: a -> a

class Round a | Ord, Field a where
    round :: a -> b | Domain b
    truncate :: a -> b | Domain b
    floor :: a -> b | Domain b
    ceil :: a -> b | Domain b

instance Semigroup Nat
instance Monoid Nat
instance Semiring Nat

instance Semigroup Int
instance Monoid Int
instance Group Int
instance Semiring Int
instance Ring Int
instance Domain Int

instance Semigroup Ratio
instance Monoid Ratio
instance Group Ratio
instance Semiring Ratio
instance Ring Ratio
instance Field Ratio
instance Round Ratio

instance Semigroup Real
instance Monoid Real
instance Group Real
instance Semiring Real
instance Ring Real
instance Field Real
instance Round Real
instance Transcedental Real

instance Semigroup (Complex a) | Semigroup a
instance Monoid (Complex a) | Monoid a
instance Group (Complex a) | Group a
instance Semiring (Complex a) | Semiring a
instance Ring (Complex a) | Ring a
instance Field (Complex a) | Field a
instance Transcedental (Complex a) | Transcedental a

instance Semigroup (Vector a) | Semigroup a
instance Monoid (Vector a) | Monoid a
instance Group (Vector a) | Group a
instance Module a (Vector a) | Ring a

/// # Casting

module Data.Cast

class Cast a b where
    cast :: a -> b

instance Cast a a where
    cast = id

instance Cast Int Real
instance Cast Real Int
instance Cast Char Int
instance Cast Int Char

/// # Functors and Monads

module Control.Functor

class Functor f where
    (<$>) infixl 4 :: (a -> b) (f a) -> f b

(<$) infixl 4 :: a (f b) -> f a | Functor f
($>) infixl 4 :: (f a) b -> f b | Functor f
(<$$>) infixl 4 :== flip (<$>)
void :: f a -> f () | Functor f


module Control.Applicative

class Applicative f | Functor f where
    pure :: a -> f a
    (<*>) infixl 4 :: (f (a -> b)) (f a) -> f b

(<*) infixl 4 :: (f a) -> (f b) -> f b | Applicative f
(<*) a b = const <$> a <*> b

(*>) infixl 4 :: (f a) -> (f b) -> f a | Applicative f
(*>) a b = const id <$> a <*> b

(<**>) infixl 4 :== flip <*>

when :: Bool (f ()) -> f () | Applicative f
when True f = f
when False _ = pure ()

unless :== flip when

lift1 :: (a -> b) (f a) -> f b | Applicative f
lift1 f a = f <$> a

lift2 :: (a b -> c) (f a) (f b) -> f c | Applicative f
lift2 f a b = f <$> a <*> b

lift3 :: (a b c -> d) (f a) (f b) (f c) -> f d | Applicative f
lift3 f a b c = f <$> a <*> b <*> c


module Control.Alternative

class Alternative f | Applicative f where
    empty :: f a
    (<|>) infixl 3 :: (f a) (f a) -> f a

some :: (f a) -> f [a] | Alternative f
many :: (f a) -> f [a] | Alternative f
count :: Int (f a) -> f [a] | Alternative f
optional :: (f a) -> f (Maybe a) | Alternative f

guard :: Bool -> f () | Alternative f
guard a = a ? pure () $ empty

choice :: (t (f a)) -> f a | Foldable t & Alternative f
choice x = foldr (<|>) empty x

choiceMap :: (a -> f b) (t a) -> f b | Foldable t & Alternative f
choiceMap f x = foldr (\x y => f x <|> y) empty x

module Control.Monad

class Monad m | Applicative m where
    (>>=) infixl 1 :: (m a) (a -> m b) -> m b

return :== pure
join :: m (m a) -> m a // flatten?
(>>|) infixl 1 :== (*>)
(|<<) infixl 1 :== (<*)
(=<<) infixr 1 :== flip (>>=)
(>=>) infixr 1 :: (a -> m b) (b -> m c) a -> m c
(<=<) infixr 1 :== flip (>=>)

/// # Collections

/// ## Foldable

module Data.Foldable

class Foldable t where
  foldr :: (a b -> b) b (t a) -> b
  foldl :: (a b -> a) a (t b) -> a

foldr1 :: (a a -> a) (t a) -> a
foldl1 :: (a a -> a) (t a) -> a

concat :: (t m) -> m | Foldable t & Monoid m
sum :== concat
product :: (t a) -> a | Foldable t & Semiring a
concatMap :: (a -> m) (t a) -> m | Foldable t & Monoid m

null :: (t a) -> Bool | Foldable t
size :: (t a) -> Int | Foldable t
elem :: a (t a) -> Bool | Foldable t, Eq a

and :: t Bool -> Bool | Foldable t
or ::  t Bool -> Bool | Foldable t
any :: (a -> Bool) (t a) -> Bool | Foldable t
all :: (a -> Bool) (t a) -> Bool | Foldable t

/// ## Traversable

module Data.Traversable

class Traversable t | Functor t & Foldable t where
  traverse :: (a -> f b) (t a) -> f (t b) | Applicative f

sequence :: t (f a) -> f (t a) | Traversable t & Applicative f
for :== flip traverse

traverse_ :: (a -> f b) (t a) -> f () | Foldable t & Applicative f
sequence_ :: t (f a) -> f ()| Foldable t & Applicative f
for_ :== flip traverse_

/// # Sliceable

module Data.Sliceable

class Sliceable s | Foldable s where
    (%) infixl 9 :: a (Int,Int) -> a

head :: (s a) -> Maybe a | Sliceable s
tail :: (s a) -> Maybe (s a) | Sliceable s
init :: (s a) -> Maybe (s a) | Sliceable s
last :: (s a) -> Maybe a | Sliceable s

uncons :: (s a) -> Maybe (a,(s a)) | Sliceable s
unsnoc :: (s a) -> Maybe ((s a),a) | Sliceable s

unsafeHead :: (s a) -> a | Sliceable s
unsafeTail :: (s a) -> (s a) | Sliceable s
unsafeInit :: (s a) -> (s a) | Sliceable s
unsafeLast :: (s a) -> a | Sliceable s

unsafeUncons :: (s a) -> (a,(s a)) | Sliceable s
unsafeUnsnoc :: (s a) -> ((s a),a) | Sliceable s

take :: Int (s a) -> (s a) | Sliceable s
drop :: Int (s a) -> (s a) | Sliceable s
split :: Int (s a) -> ((s a), (s a)) | Sliceable s

takeTill :: (a -> Bool) (s a) -> (s a) | Sliceable s & Eq a
dropTill :: (a -> Bool) (s a) -> (s a) | Sliceable s & Eq a
break :: (a -> Bool) (s a) -> ((s a), (s a)) | Sliceable s & Eq a

takeWhile :: (a -> Bool) (s a) -> (s a) | Sliceable s & Eq a
dropWhile :: (a -> Bool) (s a) -> (s a) | Sliceable s & Eq a
span :: (a -> Bool) (s a) -> ((s a), (s a)) | Sliceable s & Eq a

isPrefixOf :: (s a) (s a) -> Bool | Sliceable s & Eq a
isSuffixOf :: (s a) (s a) -> Bool | Sliceable s & Eq a
isInfixOf :: (s a) (s a) -> Bool | Sliceable s & Eq a

instance Sliceable []
instance Sliceable {} // and thuse String
instance Sliceable Slice

/// # Literals

module Data.Int

class IsInt a where
    fromInt :: Int -> a

module Data.Ratio

class IsRatio a where
    fromRatio :: Ratio -> a

module Data.String

class IsString a where
    fromString :: String -> a

module Data.List

class IsList a where
    fromList :: [a] -> a

