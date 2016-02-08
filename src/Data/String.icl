implementation module Data.String

import Algebra.Order
import Algebra.Group

import Data.Int//FIXME needed for `inc`

import _SystemArray

/// # Definition

pack :: ![Char] -> String
pack cs = {c \\ c <- cs }

unpack :: !String -> [Char]
unpack cs = [c \\ c <-: cs]

/// # Instances
