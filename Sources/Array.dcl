definition module Array

from Comparable import class Equatable, class Comparable

from Appendable import class Appendable

from Showable import class Showable

import _SystemArray

/// # Definition

// :: {}
//BUILTIN

/// # Instances

instance Showable {a} | Showable a

instance Equatable {a} | Equatable a
instance Comparable {a} | Comparable a

instance Appendable {a}
