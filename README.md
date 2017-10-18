# Yaya

Yet another … yet another recursion scheme library for Haskell.

## Some (hopefully) helpful guidelines

Greek characters (and names) for things can often add confusion, however, there are some that we’ve kept here because I think (with the right mnemonic) they are actually clarifying.

- `φ` – an algebra – “phi” (pronounced “fii” or “fee”)
- `ψ` – a coalgebra – “psi” (pronounced “sii” or “see”)

These are the symbols used in “the literature”, but I think they also provide a good visual mnemonic – φ, like an algebra, is folded inward, while ψ, like a coalgebra, opens up. So, I find these symbols more evocative than `f` and `g` or `algebra` and `coalgebra`, or any other pair of names I’ve come across for these concepts.

There are two other names, `Mu` and `Nu` (for the inductive and coinductive fixed-point operators), that I _don’t_ think have earned their place, but I just haven’t come up with something more helpful yet.

## Differences from [recursion-schemes](https://github.com/ekmett/recursion-schemes)

* uses multi-parameter type classes rather than a type family for the `Base` functor, because the latter frequently requires constraints in the form of `Recursive t, Base t ~ f`, so we prefer `Recursive t f`.
* total operations, with anything potentially partial relegated to the `yaya-unsafe` package, which has consequences:
  * `Recursive (Mu f) f` and `Corecursive (Nu f) f` are the only “safe” instances of recursive data types
* a separate `Cursive` class with `project` and `embed`, increasing the number of operations that can be defined without needing unsafe instances.
* relatively few operations are defined, with the intent being that `g{cata|ana}` be used directly instead. The more commonly known operations are provided in the `Zoo` modules, with the intent that they provide a mapping from names you may have heard to how Yaya expects you to accomplish them.

**NB**: There are a number of instances (e.g., `Corecursive [a]`) that _are_ actually safe, but they rely on Haskell’s own recursion. We could potentially add a module/package in between the safe and unsafe ones, containing `Corecursive` instances for types that are lazy in their recursive parameters and `Recursive` instances for ones that are strict.

Non-philosophical differences:
* no TH yet

## Differences from [compdata](https://github.com/pa-ba/compdata)

I’m not as familiar with compdata, so I’ll have to look at it more before fleshing this out.

Non-philosophical differences
* no type-indexed recursion schemes
