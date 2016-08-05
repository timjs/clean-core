definition module Array

from Comparable import class Eq, class Ord

from Appendable import class Appendable

from Showable import class Showable

import _SystemArray

/// # Definition

// :: {}
//BUILTIN

/// # Instances

instance Showable {a} | Showable a

instance Eq {a} | Eq a
instance Ord {a} | Ord a

instance Appendable {a}
