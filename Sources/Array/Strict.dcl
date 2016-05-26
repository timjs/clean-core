definition module Array.Strict

from Compare import class Eq, class Ord

from Append import class Append

from Show import class Show

import _SystemArray

/// # Definition

// :: {! }
//BUILTIN

/// # Instances

instance Show {!a} | Show a

instance Eq {!a} | Eq a
instance Ord {!a} | Ord a

instance Append {!a}
