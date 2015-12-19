implementation module Data.Range

import Data.Nat
import Data.Int

import Algebra.Order
import Control.Function

import Algebra.Group

/// # Definition

:: Range :== Range_

(..<) infix 5 :: !Nat !Nat -> Range
(..<) n m
    // Done by Data.Nat.nat
    // | n < 0 = abort "Data.Range.(..<): start smaller than zero"
    // | m < 0 = abort "Data.Range.(..<): end smaller than zero"
    | n > m = abort "Data.Range.(..<): start bigger than end"
    | otherwise = { start = n, end = m }

(...) infix 5 :: !Nat !Nat -> Range
(...) n m = n ..< m + nat 1

(>..) infix 5 :: !Nat !Nat -> Range
(>..) n m = n .- nat 1 ..< m + nat 1

(>..<) infix 5 :: !Nat !Nat -> Range
(>..<) n m = n .- nat 1 ..< m

start :: !Range -> Nat
start r = r.start

end :: !Range -> Nat
end r = r.end

