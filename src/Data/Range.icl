implementation module Data.Range

import Data.Nat
import Data.Int
import Data.Record.Strict

import Control.Compare
import Control.Function

import Algebra.Group

/// # Definition

:: Range :== Pair Nat Nat
// :: Range =
//     { start :: !Nat
//     , end :: !Nat
//     }

(..<) infix 5 :: !Int !Int -> Range
(..<) n m
    // Done by Data.Nat.nat
    // | n < 0 = abort "Data.Range.(..<): start smaller than zero"
    // | m < 0 = abort "Data.Range.(..<): end smaller than zero"
    | n > m = abort "Data.Range.(..<): start bigger than end"
    | otherwise = { Pair | x = nat n, y = nat m }

(...) infix 5 :: !Int !Int -> Range
(...) n m = n ..< m + 1

(>..) infix 5 :: !Int !Int -> Range
(>..) n m = n - 1 ..< m + 1

(>..<) infix 5 :: !Int !Int -> Range
(>..<) n m = n - 1 ..< m

start :: !Range -> Nat
start r = r.Pair.x

end :: !Range -> Nat
end r = r.Pair.y
