implementation module Compare

import Bool

:: Ordering :== Int

Lesser :: Ordering
Lesser = -1

Equal :: Ordering
Equal = 0

Greater :: Ordering
Greater = 1

/// # Equality

/// # Order

/// # Helpers

comparing :: !(b -> a) b b -> Ordering | Ord a
comparing p x y = compare (p x) (p y)
