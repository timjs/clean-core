implementation module Comparable

import Bool

:: Ordering :== Int

Less :: Ordering
Less = -1

Equal :: Ordering
Equal = 0

Greater :: Ordering
Greater = 1

/// # Equality

/// # Comparable

/// # Helpers

comparing :: !(b -> a) b b -> Ordering | Comparable a
comparing p x y = compare (p x) (p y)
