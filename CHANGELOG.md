1.2.2
-----
* Relaxed upper version bound for `transformers`.

1.2.1
-----
* Explicitly mark `Data.StateVar` as Safe (or Trustworthy for GHC before 7.10).

1.2
---
* Added instances for `ForeignPtr`.

1.1.1.1
-------
* Relaxed upper version bound for `stm`.

1.1.1.0
-------
* Track recent move of `Contravariant` to `base`.

1.1.0.4
-------
* Corrected HasUpdate's superclass constraint.

1.1.0.3
-------
* Removed a couple of redundant typeclass constraints.

1.1.0.2
-------
* Relaxed upper version bound for `transformers`.

1.1.0.1
-------
* Documentation changes only.

1.1.0.0
-------
* Melded the API of `foreign-var` 0.1 with the API of `StateVar` 1.0.1.1
* Introduced `HasUpdate`, which permits a wider array of uses of these combinators, including usecases that must update atomically.
* Switched to multi-parameter typeclasses. This permits `Ptr a` to be directly employed as an instance of `HasGetter`, `HasUpdate`, and `HasSetter`.

1.0.1.1
-------
* Infrastructure changes only.

1.0.1.0
-------
* Exposed `GettableStateVar`, `SettableStateVar` and `StateVar` constructors to make writing own instances possible.
* Added `Functor`, `Applicative` and `Monad` instances for `GettableStateVar`.
* Various infrastructure improvements.

1.0.0.0
-------
* Initial release.
