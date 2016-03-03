system module Data.Function

// id :: a -> a
id a :== a

// const :: a b -> a
const a b :== a

// fix :: (a -> a) a
fix f :== x
    where x = f x

($) infixr 2 //0 :: (a -> b) a -> b
($) f a :== f a

($$) infixl 3 //1 :: a (a -> b) -> b
($$) a f :== f a

(?) infixr 3 //1 :: Bool a a -> a
(?) b t f :== if b t f

(o) infixr 9 // :: (b -> c) (a -> b) a -> c
(o) f g x :== f (g x)

// (<|) infixr 2 //:: (a -> b) a -> b
// (<|) f a :== f a

// (|>) infixl 3 //:: a (a -> b) -> b
// (|>) a f :== f a

// (<<) infixr 9 //:: (b -> c) (a -> b) a -> c
// (<<) f g x :== f (g x)

// (>>) infixl 8 //:: (a -> b) (b -> c) a -> c
// (>>) f g x :== f (g x)

// flip :: (a b -> c) -> (b a -> c)
flip f :== g
    where g b a = f a b

// curry :: ((a, b) -> c) a b -> c
curry f a b :== f (a,b)

// uncurry :: (a b -> c) (a,b) -> c
uncurry f (a,b) :== f a b

(`on`) infixl 0 //:: (b b -> c) (a -> b) a a -> c
(`on`) g f :== \x y -> g (f x) (f y)

abort :: !String -> .a
undefined :: .a
unreachable :: .a
