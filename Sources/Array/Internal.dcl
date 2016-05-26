definition module Array.Internal

import Bool
import Char
// import Nat
import Int
import Real

import Enum
import Array.Unboxed

import Compare
import Num

import Append

import Show

import _SystemArray

/// # Macros

// showArray :: String {a} -> String | Show a
showArray symbol xs
    | size xs == 0 :== "{" ++ symbol ++ "}"
    | otherwise    :== "{" ++ symbol ++ show xs.[0] ++ go 1
    where
        go i
            | i < size xs = "," ++ show xs.[i] ++ go (succ i)
            | otherwise   = "}"

// eqArray :: {a} {a} -> Bool | Eq a
eqArray xs ys
    | size xs /= size ys :== False
    | size xs == 0       :== True
    | otherwise          :== go 0
    where
        go i
            | i < size xs = xs.[i] == ys.[i] && go (succ i)
            | otherwise   = True

// ltArray :: {a} {a} -> Bool | Ord a
ltArray xs ys
    | size xs > size ys :== False
    | otherwise         :== go 0
    where
        go i
            | i < size xs
                | xs.[i] <  ys.[i] = True
                | xs.[i] == ys.[i] = go (succ i)
                | otherwise        = False
            | otherwise = size xs < size ys

// concatArray :: {a} {a} -> {a}
concatArray xs ys
    // new = array (size xs + size ys) neutral
    # new = unsafeArray (size xs + size ys)
    # new = { new & [i]           = xs.[i] \\ i <- [0..pred (size xs)] }
    # new = { new & [i + size xs] = ys.[i] \\ i <- [0..pred (size ys)] }
    :== new

// emptyArray :: {a}
emptyArray :== { }
