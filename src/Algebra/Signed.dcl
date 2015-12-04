definition module Algebra.Signed

:: Sign = Positive | Zero | Negative

class Signed a where
    signum :: !a -> Sign

