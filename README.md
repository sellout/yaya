# Yaya

Yet another … yet another recursion scheme library for Haskell.

## Overview

Recursion schemes allow you to separate _any_ recursion from your business logic, writing step-wise operations that can be applied in a way that guarantees termination (or, dually, progress).

How is this possible? You can’t have totality _and_ Turing-completeness, can you? Oh, but [you can](https://pdfs.semanticscholar.org/e291/5b546b9039a8cf8f28e0b814f6502630239f.pdf) – there is a particular type, `Partial a` (encoded with a fixed-point) that handles potential non-termination, akin to the way that `Maybe a` handles exceptional cases. It can be folded into `IO` in your main function, so that the runtime can execute a Turing-complete program that was modeled totally.

##  organization

* [`yaya`](core/README.md) – the safe operations and data types for working with recursion
* [`yaya-hedgehog`](hedgehog/README.md) – utilities for testing your Yaya-using code with [Hedgehog](https://github.com/hedgehogqa/haskell-hedgehog)
* [`yaya-unsafe`](unsafe/README.md) – unsafe instances and operations

## other libraries

### [Turtles](https://github.com/sellout/turtles)

Yaya is a sister library to Turtles – the same approach, but implemented in
Scala. Here are some differences to be aware of:

* the `Zoo` modules in Turtles are both larger and their use is more encouraged,
  because Scala’s inference makes it harder to use `gcata` etc. directly;
* the `Unsafe` and `Native` modules have different contents, because different
  structures are strict or lazy between the two languages. E.g., in Scala,
  `scala.collection.immutable.List` is strict, so the `Recursive` instance is in
  `Native`, while the `Corecursive` instance is in `Unsafe`, but Haskell’s
  `Data.List` is lazy, so the `Corecursive` instance is in `Native` while the
  `Recursive` instance is in `Unsafe`.

### Differences from [recursion-schemes](https://github.com/ekmett/recursion-schemes)

#### poly-kinded folds

The `c` type parameter specifies the arrow to use, so while it's common to
specialize to `(->)`, other options can give you polymorphic recursion over
nested data types (e.g., GADTs). Among other things, you can use this to define
folds of fixed-sized structures:

```haskell
data VectF elem a (i :: Nat) where
  EmptyVect :: VectF elem a 0
  VCons :: KnownNat n => elem -> a i -> VectF elem a (n + 1)

type Vect elem n = HMu (VectF elem) n
```

#### bias for totality

Yaya tries to encourage you to define things in ways that are likely to maintain
promises of termination. In some cases, the compiler can even tell you when
you've broken these promises, but it falls short of any guarantee of totality.

Anything known to be partial is relegated to the `yaya-unsafe` package -- mostly
useful when you're in the process of converting existing directly-recursive
code.

**NB**: There are a number of instances (e.g., `Corecursive [a] (XNor a)`) that
_are_ actually safe, but they rely on Haskell’s own recursion. We could
potentially add a module/package in between the safe and unsafe ones, containing
`Corecursive` instances for types that are lazy in their recursive parameters
and `Recursive` instances for ones that are strict.

#### bias for working with algebras

We try to provide fewer fold operations (although all the usual ones can be
found in the `Zoo` modules). Instead, we expect more usage of `gcata`, and we
provide a collection of "algebra transformers" to make it easier to transform
various functions into generalized algebras, and between different generalized
algebras to maximize the opportunities for fusion. Although, more importantly,
it allows you to write "proto-algebras", which are functions that you expect to
use in a fold but that aren't strictly in the shape of an algebra.

#### productive metamorphisms

Yaya has productive metamorphisms (see `streamAna` and `streamGApo` -- also
`stream` and `fstream` for more specialized versions). The naïve composition of
`cata` and `ana` has no benefits.

#### more atomic classes

recursion-schemes combines `cata` and `project` into a single type
class. However, the laws for `project` require either `embed` or `ana`, never
`cata`. Similarly, the laws for `cata` either stand alone or require `embed`,
never `project`. And you can restate this paragraph, replacing each operation
with its dual.

Also, it is impossible to define `embed` for some pattern functors where it's
still possible to define `project`, so `project` and `embed` need to be
independent.

One unfortunate consequence of the above conditions is that `Projectable` is
lawless on its own. However, we expect there to be a corresponding instance of
either `Corecursive` or `Steppable` in all cases.

#### multi-parameter type classes

A purely ergonomic difference, yaya uses multi-parameter type classes instead of
a `Base` type family.

The latter frequently requires constraints in the form of
`(Recursive t, Base t ~ f)`, so we prefer `Recursive t f`.

#### naming

Pattern functors and algebras tend to be named independently of their
fixed-points. E.g., we use `Maybe` directly instead of some `NatF`, `XNor a b`
instead of `ListF`, and ``a `AndMaybe` b`` instead of `NonEmptyF`.

This is because many pattern functors and algebras can be applied differently in
different situations, so we try to avoid pigeon-holing them and rather trying to
understand what the definition itself means, rather than in the context of a
fold.

### Differences from [compdata](https://github.com/pa-ba/compdata)

I’m not as familiar with compdata, so I’ll have to look at it more before
fleshing this out.

* poly-kinded recursion schemes instead of separate classes for type-indexed
  recursion schemes. Using `PolyKinds` also allows for a wider variety of folds,
  e.g., where the type index has kind `Type -> Type` rather than kind `Type`.
