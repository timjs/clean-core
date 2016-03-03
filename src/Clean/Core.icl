implementation module Clean.Core

import Clean.Core

c :: {#Int}
c = {1, 2}

d :: {#Int}
d = {3, 4}

// e :: {#Nat}
// e = {nat 1, nat 2}

:: NewInt = {n :: Int}

instance Semigroup {#NewInt} where
    (+) xs ys = concatUnboxedArray xs ys

// concatUnboxedArray :: {#a} {#a} -> {#a}
concatUnboxedArray xs ys #
    // new = array (size xs + size ys) neutral
    new = unsafeArray (size xs + size ys)
    new = { new & [i]           = xs.[i] \\ i <- [0..pred (size xs)] }
    new = { new & [i + size xs] = ys.[i] \\ i <- [0..pred (size ys)] }
  :== new

f :: {#NewInt}
f = {{n = 5}, {n = 6}}

g :: {#NewInt}
g = {{n = 7}, {n = 8}}

n0 = nat 0
n1 = nat 1
n2 = nat 2
n_ = nat -1

Start = [n2 .- n1, n1 .- n0, n1 .- n2, n_]
