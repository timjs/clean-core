system module _DataArrayInternal

from Data.Nat import :: Nat

/// # Overloading

class Array .a e where
    // creation
    array :: !Nat e -> *(a e)
    _array :: !Nat -> *(a .e)
    // a.size and a!size TODO add syntax
    size :: !.(a .e) -> Nat
    usize :: !u:(a .e) -> *(!Nat, !u:(a .e))
    // a.[i] and a![i]
    select :: !.(a .e) !Nat -> .e
    uselect :: !u:(a e) !Nat -> *(e, !u:(a e))
    // a.[i..<j] and a![i..<j] TODO add syntax
    slice :: !.(a .e) !Nat !Nat -> .(a .e)
    uslice :: !u:(a .e) !Nat !Nat -> *(.(a .e), !u:(a .e))
    // a.[i] = e
    update  :: !*(a .e) !Nat .e -> *(a .e)
    replace :: !*(a .e) !Nat .e -> *(.e, !*(a .e))
    // efficient array operations
    concat :: !.(a .e) !.(a .e) -> .(a .e)
    compare :: !.(a .e) !.(a .e) -> Bool


//FIXME change name in compiler?
createArray x y :== array x y
_createArray x :== _array x

instance Array {} a
