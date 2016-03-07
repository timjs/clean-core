implementation module Control.Foldable

import Data.Function
import Data.Bool
import Data.Nat
import Data.Maybe

import Algebra.Order

import Algebra.Group
import Algebra.Ring

/// # Class

/// A variant of `foldr` that has no base case,
/// and thus may only be applied to non-empty structures.
foldr1 :: (a a -> a) (t a) -> a | Foldable t
foldr1 f xs = default (abort "Control.Foldable.foldr1: empty structure") (foldr mf Nothing xs)
    where
        mf x m = Just (case m of
            Nothing -> x
            Just y  -> f x y)

/// A variant of 'foldl' that has no base case,
/// and thus may only be applied to non-empty structures.
foldl1 :: (a a -> a) (t a) -> a | Foldable t
foldl1 f xs = default (abort "Control.Foldable.foldl1: empty structure") (foldl mf Nothing xs)
    where
        mf m y = Just (case m of
            Nothing -> y
            Just x  -> f x y)

/// Combine each element of a structure into a monoid
fold :: (t m) -> m | Foldable t & Monoid m
fold xs = foldr (+) neutral xs

/// Combine into a monoid the collective results of applying a function
/// to each element of a structure
foldMap :: (a -> m) (t a) -> m | Foldable t & Monoid m
foldMap f xs = foldr ((+) o f) neutral xs

defaultFoldr` :: (a b -> b) b (t a) -> b | Foldable t
defaultFoldr` f z0 l = foldl f` id l z0
    where
        f` k x z = k (f x z)

defaultFoldl` :: (b a -> b) b (t a) -> b | Foldable t
defaultFoldl` f z0 l = foldr f` id l z0
    where
        f` x k z = k (f z x)

/// # Special folds

/// Test whether the structure is empty. The default implementation is
/// optimized for structures that are similar to cons-lists, because there
/// is no general way to do better.
null :: (t a) -> Bool | Foldable t
null xs = foldr (\_ _ -> False) True xs

/// Returns the length of a finite structure as an `Nat`.  The
/// default implementation is optimized for structures that are similar to
/// cons-lists, because there is no general way to do better.
length :: (t a) -> Nat | Foldable t
length xs = foldl (\c _ -> c + (nat 1)) (nat 0) xs

// Does the element occur in the structure?
elem :: a (t a) -> Bool | Foldable t & Eq a
elem x y = any (\z -> x == z) y

// `notElem` is the negation of `elem`.
notElem :: a (t a) -> Bool | Foldable t & Eq a
notElem x y = not (elem x y)

// The `find` function takes a predicate and a structure and returns
// the leftmost element of the structure matching the predicate, or
// `Nothing` if there is no such element.
find :: (a -> Bool) (t a) -> Maybe a | Foldable t
find p x = undefined //listToMaybe (foldMap (\x -> if (p x) [x] []) x)

/// ## Ord

// The largest element of a non-empty structure.
maximum :: (t a) -> a | Foldable t & Ord a
maximum x = foldr1 max x

// The largest element of a non-empty structure with respect to the
// given greater-than function.
maximumBy :: (a a -> Bool) (t a) -> a | Foldable t
maximumBy p x = foldr1 max` x
    where max` x y = if (p x y) x y

// The least element of a non-empty structure.
minimum :: (t a) -> a | Foldable t & Ord a
minimum x = foldr1 min x

// The least element of a non-empty structure with respect to the
// given lesser-than function.
minimumBy :: (a a -> Bool) (t a) -> a | Foldable t
minimumBy p x = foldr1 min` x
    where min` x y = if (p x y) x y

/// ## Lists

/// The concatenation of all the elements of a container of lists.
/// This is a synonym for `fold`
concat :: (t [a]) -> [a] | Foldable t
concat xs = undefined//fold xs

/// Map a function over all the elements of a container and concatenate
/// the resulting lists.
/// This is a synonym for `foldMap`
concatMap :: (a -> [b]) (t a) -> [b] | Foldable t
concatMap f xs = undefined//foldMap f xs

/// ## Monoids and Semirings

/// Add together all the elements of a structure
/// This is a synonym for `fold`
sum :: (t m) -> m | Foldable t & Monoid m
sum xs = foldr (+) neutral xs

/// Multiply together all elements of a structure
product :: (t a) -> a | Foldable t & Semiring a
product xs = foldr (*) unity xs

/// ## Boolean

/// The conjunction of all elements of a structure containing
/// lazy boolean values. `and` short-circuits from left to right,
/// evaluating until either an element is `False` or no elements remain.
and :: (t Bool) -> Bool | Foldable t
and xs = foldl (&&) True xs

/// The disjunction of all elements of a structure containing
/// lazy boolean values. `or` short-circuits from left to right, evaluating
/// either until an element is `True` or no elements remain.
or ::  (t Bool) -> Bool | Foldable t
or xs = foldl (||) False xs

/// The disjunction of the collective results of applying a
/// predicate to all elements of a structure. `any` short-circuits
/// from left to right.
any :: (a -> Bool) (t a) -> Bool | Foldable t
any p xs = foldl (\x y -> x || p y) False xs

/// The disjunction of the collective results of applying a
/// predicate to all elements of a structure. `all` short-circuits
/// from left to right.
all :: (a -> Bool) (t a) -> Bool | Foldable t
all p xs = foldl (\x y -> x && p y) True xs
