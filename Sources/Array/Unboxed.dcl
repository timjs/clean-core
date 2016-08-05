system module Array.Unboxed

from Comparable import class Eq, class Ord

from Appendable import class Appendable

from Showable import class Showable

import Array.Internal

/// # Definition

// :: {# }
//BUILTIN

/// # Instances

instance Showable {#e} | Showable, Unboxed e

instance Eq {#e} | Eq, Unboxed e
instance Ord {#e} | Ord, Unboxed e

instance Appendable {#e} | Unboxed e

/// # Specializations

/// - Note: these functions *have to be* strict!
///   Otherwise the compiler can't infer strictness of the default, non-strict
///   `show` and `(++)`.
class Unboxed e where
    showUnboxedArray :: !{#e} -> String | Showable e
    showUnboxedArray xs = showArray "#" xs

    eqUnboxedArray :: !{#e} !{#e} -> Bool | Eq e
    eqUnboxedArray xs ys = eqArray xs ys

    ltUnboxedArray :: !{#e} !{#e} -> Bool | Ord e
    ltUnboxedArray xs ys = ltArray xs ys

    concatUnboxedArray :: !{#e} !{#e} -> {#e}
    concatUnboxedArray xs ys = concatArray xs ys

    emptyUnboxedArray :: {#e}
    emptyUnboxedArray = emptyArray

instance Unboxed Bool
instance Unboxed Char
// instance Unboxed Nat
instance Unboxed Int
instance Unboxed Real
