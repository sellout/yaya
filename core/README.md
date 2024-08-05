# Yaya

[![Packaging status](https://repology.org/badge/tiny-repos/haskell:yaya.svg)](https://repology.org/project/haskell:yaya/versions)
[![latest packaged versions](https://repology.org/badge/latest-versions/haskell:yaya.svg)](https://repology.org/project/haskell:yaya/versions)

Yet another … yet another recursion scheme library for Haskell

## overview

Recursion schemes allow you to separate _any_ recursion from your business logic, writing step-wise operations that can be applied in a way that guarantees termination (or, dually, progress).

How's this possible? You can’t have totality _and_ Turing-completeness, can you? Oh, but [you can](https://pdfs.semanticscholar.org/e291/5b546b9039a8cf8f28e0b814f6502630239f.pdf) – there is a particular type, `Partial a` (encoded with a fixed-point) that handles potential non-termination, akin to the way that `Maybe a` handles exceptional cases. It can be folded into `IO` in your main function, so that the runtime can execute a Turing-complete program that was modeled totally.

**NB**: The tests for this package are unfortunately included in `yaya-hedgehog` instead, to avoid a dependency cycle.

## organization

This organization is intended to make this a lightly-opinionated library. You should only need to import one module (per package) into any module of yours.

- `Pattern` – This is what you should use most of the time. It provides common pattern functors that aren’t found elsewhere as well as other operations that are useful when you’re defining your own algebras.
- `Fold` – This (and its submodules) provides algebra transformers, fixed-point operators, and other things you use when applying folds.
- `Retrofit` – Utilities for making your existing data types compatible with recursion schemes.
- `Applied` – A number of commonly-useful utilities defined as folds. Intended both as examples and code that you can actually use in your projects.
- `Zoo` – Names that you may have seen in the recursion scheme literature, but that we generally avoid using here. In general, prefer the right-hand side of these definitions, which shouldn’t require importing this module.

## Some (hopefully) helpful guidelines

Greek characters (and names) for things can often add confusion, however, there are some that we’ve kept here because I think (with the right mnemonic) they're actually clarifying.

- `φ` – an algebra – “phi” (pronounced /faɪ/)
- `ψ` – a coalgebra – “psi” (pronounced /ˈ(p)saɪ/)

These are the symbols used in “the literature”, but I think they also provide a good visual mnemonic – φ, like an algebra, is folded inward, while ψ, like a coalgebra, opens up. So, I find these symbols more evocative than `f` and `g` or `algebra` and `coalgebra`, or any other pair of names I’ve come across for these concepts.

There are two other names, `Mu` and `Nu` (for the inductive and coinductive fixed-point operators), that I _don’t_ think have earned their place, but I just haven’t come up with something more helpful yet.

### Naming Conventions

There's a set of conventions around the naming of the operations. There are many variants of each operation (and they're all ultimately variants of `cata` and `ana`), so understanding this convention should help make it easier to understand the myriad possibilities rather than learning them by rote. The general pattern is

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

As a mnemonic, you can read the `e` as “exterior” as with a regular generalized fold, the `x` is on the _inside_ of the `f`, while with the Elgot variant, it's on the _outside_ of the `f`.

#### `T`

“Transformer” variant – For some fold that takes an algebra like `f (x a) -> a`, and where `t` is the (monad or comonad) transformer of `x`, the transformer variant takes an algebra like `f (t m a) -> a`.

#### `M`

Kleisli (“monadic”) variant – This convention is much more widespread than simply recursion schemes. A fold that returns its result in a `Monad`, by applying a Kleisli algebra (that is, `f a -> m a` rather than `f a -> a`. The dual of this might be something like `anaW` (taking a seed value in a `Comonad`), but those are uninteresting. Having Kleisli variants of unfolds is unsafe, as it can force traversal of an infinite structure. If you’re looking for an operation like that, you are better off with an effectful streaming library.

## comparisons

Other projects similar to this one, and how they differ.

### [Turtles](https://github.com/sellout/turtles)

**This project has been deprecated. Check out [Droste](https://github.com/higherkindness/droste) instead.**

Yaya is a sister library to Turtles – the same approach, but implemented in
Scala. Here are some differences to be aware of:

- the `Zoo` modules in Turtles are both larger and their use is more encouraged,
  because Scala’s inference makes it harder to use `gcata` etc. directly;
- the `Unsafe` and `Native` modules have different contents, because different
  structures are strict or lazy between the two languages. For example., in Scala,
  `scala.collection.immutable.List` is strict, so the `Recursive` instance is in
  `Native`, while the `Corecursive` instance is in `Unsafe`, but Haskell’s
  `Data.List` is lazy, so the `Corecursive` instance is in `Native` while the
  `Recursive` instance is in `Unsafe`.

### [recursion-schemes](https://github.com/ekmett/recursion-schemes)

#### poly-kinded folds

The `c` type parameter specifies the arrow to use, so while it's common to
specialize to `(->)`, other options can give you polymorphic recursion over
nested data types (for example., GADTs). Among other things, you can use this to define
folds of fixed-sized structures:

```haskell
data VectF elem a (i :: Nat) where
  EmptyVect :: VectF elem a 0
  VCons :: KnownNat n => elem -> a i -> VectF elem a (n + 1)

type Vect elem n = HMu (VectF elem) n
```

#### bias for totality

Yaya tries to encourage you to define things in ways that are likely to maintain
promises of termination. Sometimes, the compiler can even tell you when
you've broken these promises, but it falls short of any guarantee of totality.

Anything known to be partial is relegated to the `yaya-unsafe` package -- mostly
useful when you're in the process of converting existing directly-recursive
code.

**NB**: There are a number of instances (for example, `Corecursive [a] (XNor a)`) that
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

Also, it's impossible to define `embed` for some pattern functors where it's
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
fixed-points. For example, we use `Maybe` directly instead of some `NatF`, `XNor a b`
instead of `ListF`, and `AndMaybe a b` instead of `NonEmptyF`.

This is because many pattern functors and algebras can be applied differently in
different situations, so we try to avoid pigeon-holing them and rather trying to
understand what the definition itself means, rather than in the context of a
fold.

### [compdata](https://github.com/pa-ba/compdata)

I’m not as familiar with compdata, so I’ll have to look at it more before
fleshing this out.

- poly-kinded recursion schemes instead of separate classes for type-indexed
  recursion schemes. Using `PolyKinds` also allows for a wider variety of folds,
  for example, where the type index has kind `Type -> Type` rather than kind `Type`.
