definition module Control.Cast

/// Type class for transforming an instance of a data type to another type.
class Cast a b where
    /// Perform a cast operation from type `a` to type `b`.
    cast :: !a -> b

/*
/// # String casts

instance Cast String Int where
    cast = prim__fromStrInt

instance Cast String Double where
    cast = prim__strToFloat

instance Cast String Integer where
    cast = prim__fromStrBigInt

/// # Int casts

instance Cast Int String where
    cast = prim__toStrInt

instance Cast Int Double where
    cast = prim__toFloatInt

instance Cast Int Integer where
    cast = prim__sextInt_BigInt

/// # Double casts

instance Cast Double String where
    cast = prim__floatToStr

instance Cast Double Int where
    cast = prim__fromFloatInt

instance Cast Double Integer where
    cast = prim__fromFloatBigInt

/// # Integer casts

instance Cast Integer String where
    cast = prim__toStrBigInt

instance Cast Integer Double where
    cast = prim__toFloatBigInt

/// # Char casts

instance Cast Char Int where
    cast = prim__charToInt
*/

