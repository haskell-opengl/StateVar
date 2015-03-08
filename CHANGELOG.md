1.1
---
* Melded the API of `foreign-var` 0.1 with the API of `StateVar` 1.0.1.1
* Introduced `HasUpdate`, which permits a wider array of uses of these combinators, including usecases that must update atomically.
* Switched to multi-parameter typeclasses. This permits `Ptr a` to be directly employed as an instance of `HasGetter`, `HasUpdate`, and `HasSetter`.
