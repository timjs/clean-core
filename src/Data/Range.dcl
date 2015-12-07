definition module Data.Range

from Data.Nat import :: Nat

/// # Definition

:: Range_ = !{ start :: !Nat, end :: !Nat }
:: Range (:== Range_)

(..<) infix 5 :: !Nat !Nat -> Range
(...) infix 5 :: !Nat !Nat -> Range
(>..) infix 5 :: !Nat !Nat -> Range
(>..<) infix 5 :: !Nat !Nat -> Range

start :: !Range -> Nat
end :: !Range -> Nat

