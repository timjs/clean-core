implementation module Enum

import Comparable
import Numeral

import Int

//FIXME maybe move this module to Base, it isn't a system module...

//FIXME use specialized pred and succ on Int instead of inc and dec
inc x :== x + 1
dec x :== x - 1

/// # Default implementations

//FIXME move to Enum.succ and Enum.pred when default members are there
succ :: !a -> a | Enum a
succ x = toEnum (inc nx)
where
    nx = fromEnum x

//FIXME move to Enum.succ and Enum.pred when default members are thre
pred :: !a -> a | Enum a
pred x = toEnum (dec nx)
where
    nx = fromEnum x

enumFrom :: !a -> .[a] | Enum a
enumFrom x = enumFrom nx
where
    // enumFrom :: !Int -> .[a]
    enumFrom n = [toEnum n : enumFrom (inc n)]
    nx = fromEnum x

enumFromThen :: !a !a -> .[a] | Enum a
enumFromThen x y = [x : enumFromBy nx (ny - nx)]
where
    // enumFromBy :: !Int !Int -> .[a]
    enumFromBy n d = [toEnum n : enumFromBy (n + d) d]
	nx = fromEnum x
    ny = fromEnum y

// enumFromTo :: !a !a -> .[a] | Enum a
// enumFromTo x y
//     | nx <= ny  = [x : enumFromTo (inc x) y]
//     | otherwise = []
// where
//     nx = fromEnum x
//     ny = fromEnum y

enumFromTo :: !a !a -> .[a] | Enum a
enumFromTo x z = enumFromTo nx nz
where
    // enumFromTo :: !Int !Int -> .[a]
	enumFromTo n m
		| n <= m    = [toEnum n : enumFromTo (inc n) m]
        | otherwise = []
    nx = fromEnum x
    nz = fromEnum z

enumFromThenTo :: !a !a !a -> .[a] | Enum a
enumFromThenTo x y z
    | nx <= ny  = enumFromByUpto nx (ny - nx) nz
    | otherwise = enumFromByDownto nx (nx - ny) nz
where
    // enumFromByUpto :: !Int !Int !Int -> .[Int]
    enumFromByUpto n d m
        | n <= m    = [toEnum n : enumFromByUpto (n + d) d m]
        | otherwise = []
    // enumFromByDownto :: !Int !Int !Int -> .[Int]
    enumFromByDownto n d m
        | n >= m    = [toEnum n : enumFromByDownto (n - d) d m]
        | otherwise = []
	nx = fromEnum x
	ny = fromEnum y
	nz = fromEnum z
