system module _DataArrayUnboxedInternal

import _DataArrayInternal

/// # Overloading

instance Array {#} Bool
instance Array {#} Char
// instance Array {#} Nat
instance Array {#} Int
instance Array {#} Real

instance Array {#} {#.a}
instance Array {#} {!.a}
instance Array {#} {.a}

instance Array {#} a
