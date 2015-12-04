definition module Data.Range

from Data.Nat import :: Nat
from Data.Record.Strict import :: Pair

/// # Definition

:: Range (:== Pair Nat Nat)
//TODO future :: Range (:== { start :: !Nat, end :: !Nat })

(..<) infix 5 :: !Int !Int -> Range

start :: !Range -> Nat
end :: !Range -> Nat

