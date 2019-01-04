# Yaya

Yet another … yet another recursion scheme library for Haskell.

## Overview

Recursion schemes allow you to separate _any_ recursion from your business logic, writing step-wise operations that can be applied in a way that guarantees termination (or, dually, progress).

How is this possible? You can’t have totality _and_ Turing-completeness, can you? Oh, but [you can](https://pdfs.semanticscholar.org/e291/5b546b9039a8cf8f28e0b814f6502630239f.pdf) – there is a particular type, `Partial a` (encoded with a fixed-point) that handles potential non-termination, akin to the way that `Maybe a` handles exceptional cases. It can be folded into `IO` in your main function, so that the runtime can execute a Turing-complete program that was modeled totally.

## organization

This organization is intended to make this a lightly-opinionated library. You should only need to import one module (per package) into any module of yours.

* `Pattern` – This is what you should use most of the time. It provides common pattern functors that aren’t found elsewhere as well as other operations that are useful when you’re defining your own algebras.
* `Fold` – This (and its submodules) provides algebra transformers, fixed-point operators, and other things you use when applying folds.
* `Retrofit` – Utilities for making your existing data types compatible with recursion schemes.
* `Applied` – A number of commonly-useful utilies defined as folds. Intended both as examples and code that you can actually use in your projects.
* `Zoo` – Names that you may have seen in the recursion scheme literature, but that we generally avoid using here. In general, prefer the right-hand side of these definitions, which shouldn’t require importing this module.

## Some (hopefully) helpful guidelines

Greek characters (and names) for things can often add confusion, however, there are some that we’ve kept here because I think (with the right mnemonic) they are actually clarifying.

- `φ` – an algebra – “phi” (pronounced “fye” or “fee”)
- `ψ` – a coalgebra – “psi” (pronounced “sai” or “see”)

These are the symbols used in “the literature”, but I think they also provide a good visual mnemonic – φ, like an algebra, is folded inward, while ψ, like a coalgebra, opens up. So, I find these symbols more evocative than `f` and `g` or `algebra` and `coalgebra`, or any other pair of names I’ve come across for these concepts.

There are two other names, `Mu` and `Nu` (for the inductive and coinductive fixed-point operators), that I _don’t_ think have earned their place, but I just haven’t come up with something more helpful yet.

### Naming Conventions

There is a set of conventions around the naming of the operations. There are many variants of each operation (and they are all ultimately variants of `cata` and `ana`), so understanding this convention should help make it easier to understand the myriad possibilities rather than learning them by rote. The general pattern is

> [`e`][`g`]`operation`[`T`][`M`]

#### `g`

“Generalized” variant – This parameterizes the fold over some `DistributiveLaw` that generalizes the (co)algebra over some `Monad` or `Comonad`. This is normally only applied to the fundamental operations – `cata`, `ana`, and `hylo`, but there is also a `gapo` (dual to `zygo`) that really only coincidentally follows this naming pattern.

Many of the other “well-known” named folds are specializations of this:

- when specialized to `((,) T)`, it’s `para`;
- when `((,) B)`, `zygo`;
- when `Free f`, `futu`;
- etc.

#### `e`

“Elgot” variant – Named after the form of coalgebra used in an “Elgot algebra”. If there is an operation that takes some `f (x a) -> a`, the Elgot variant takes `x (f a) -> a`, which often has similar but distinct properties from the original.

As a mnemonic, you can read the `e` as “exterior” as with a regular generalized fold, the `x` is on the _inside_ of the `f`, while with the Elgot variant, it is on the _outside_ of the `f`.

#### `T`

“Transformer” variant – For some fold that takes an algebra like `f (x a) -> a`, and where `t` is the (monad or comonad) transformer of `x`, the transformer variant takes an algebra like `f (t m a) -> a`.

#### `M`

Kleisli (“monadic”) variant – This convention is much more widespread than simply recursion schemes. A fold that returns its result in a `Monad`, by applying a Kleisli algebra (i.e., `f a -> m a` rather than `f a -> a`. The dual of this might be something like `anaW` (taking a seed value in a `Comonad`), but those are uninteresting. Having Kleisli variants of unfolds is unsafe, as it can force traversal of an infinite structure. If you’re looking for an operation like that, you are better off with an effectful streaming library.
