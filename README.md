# Yaya

Yet another … yet another recursion scheme library for Haskell.

## Overview

Recursion schemes allow you to separate _any_ recursion from your business logic, writing step-wise operations that can be applied in a way that guarantees termination (or, dually, progress).

How is this possible? You can’t have totality _and_ Turing-completeness, can you? Oh, but [you can](https://pdfs.semanticscholar.org/e291/5b546b9039a8cf8f28e0b814f6502630239f.pdf) – there is a particular type, `Partial a` (encoded with a fixed-point) that handles potential non-termination, akin to the way that `Maybe a` handles exceptional cases. It can be folded into `IO` in your main function, so that the runtime can execute a Turing-complete program that was modeled totally.

## Some (hopefully) helpful guidelines

Greek characters (and names) for things can often add confusion, however, there are some that we’ve kept here because I think (with the right mnemonic) they are actually clarifying.

- `φ` – an algebra – “phi” (pronounced “fye” or “fee”)
- `ψ` – a coalgebra – “psi” (pronounced “sai” or “see”)

These are the symbols used in “the literature”, but I think they also provide a good visual mnemonic – φ, like an algebra, is folded inward, while ψ, like a coalgebra, opens up. So, I find these symbols more evocative than `f` and `g` or `algebra` and `coalgebra`, or any other pair of names I’ve come across for these concepts.

There are two other names, `Mu` and `Nu` (for the inductive and coinductive fixed-point operators), that I _don’t_ think have earned their place, but I just haven’t come up with something more helpful yet.

## Packages

* `yaya` – the safe operations and data types for working with recursion
* `yaya-unsafe` – unsafe instances and operations
* `yaya-optics` – lenses, prisms, and isomorphisms – oh my!

## [Turtles](https://github.com/sellout/turtles)

Yaya is a sister library to Turtles – the same approach, but implemented in Scala. Here are some differences to be aware of:

* the `Zoo` modules in Turtles are both larger and their use is more encouraged, because Scala’s inference makes it harder to use `gcata` etc. directly;
* the `Unsafe` and `Native` modules have different contents, because different structures are strict or lazy between the two languages. E.g., in Scala, `scala.collection.immutable.List` is strict, so the `Recursive` instance is in `Native`, while the `Corecursive` instance is in `Unsafe`, but Haskell’s `Data.List` is lazy, so the `Corecursive` instance is in `Native` while the `Recursive` instance is in `Unsafe`.

## Differences from [recursion-schemes](https://github.com/ekmett/recursion-schemes)

* uses multi-parameter type classes rather than a type family for the `Base` functor, because the latter frequently requires constraints in the form of `Recursive t, Base t ~ f`, so we prefer `Recursive t f`.
* total operations, with anything potentially partial relegated to the `yaya-unsafe` package, which has consequences:
  * `Recursive (Mu f) f` and `Corecursive (Nu f) f` are the only “safe” instances of recursive data types
* a separate `Steppable` class with `project` and `embed`, increasing the number of operations that can be defined without needing unsafe instances.
* relatively few operations are defined, with the intent being that `g{cata|ana}` be used directly instead. The more commonly known operations are provided in the `Zoo` modules, with the intent that they provide a mapping from names you may have heard to how Yaya expects you to accomplish them.
* pattern functors tend to be named independently of their fixed-points. E.g., we use `Maybe` directly instead of some `NatF`, `XNor a b` instead of `ListF`, and ``a `AndMaybe` b`` instead of `NonEmptyF`.

**NB**: There are a number of instances (e.g., `Corecursive [a] (XNor a)`) that _are_ actually safe, but they rely on Haskell’s own recursion. We could potentially add a module/package in between the safe and unsafe ones, containing `Corecursive` instances for types that are lazy in their recursive parameters and `Recursive` instances for ones that are strict.

Non-philosophical differences:
* no TH yet – this _might_ be a philosophical difference, actually. On the one hand, it useful to be able to migrate existing code by quickly creating a pattern functor, but on the other, this stuff is generally unsafe. So, it should at least be relegated to `yaya-unsafe`.
* Yaya has productive metamorphisms (`stream`, etc.). The naïve composition of `cata` and `ana` has no benefits, but it has been difficult to generalize Gibbons’ streaming transformers to data structures other than lists.

## Differences from [compdata](https://github.com/pa-ba/compdata)

I’m not as familiar with compdata, so I’ll have to look at it more before fleshing this out.

Non-philosophical differences
* no type-indexed recursion schemes – ideally Yaya would achieve this via PolyKinds.
