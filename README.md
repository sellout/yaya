# Yaya

[![built with garnix](https://img.shields.io/endpoint?url=https%3A%2F%2Fgarnix.io%2Fapi%2Fbadges%2Fsellout%2Fyaya)](https://garnix.io)
[![Packaging status](https://repology.org/badge/tiny-repos/haskell:yaya.svg)](https://repology.org/project/haskell:yaya/versions)
[![latest packaged versions](https://repology.org/badge/latest-versions/haskell:yaya.svg)](https://repology.org/project/haskell:yaya/versions)

Yet another … yet another recursion scheme library for Haskell

## overview

Recursion schemes allow you to separate _any_ recursion from your business logic, writing step-wise operations that can be applied in a way that guarantees termination (or, dually, progress).

How's this possible? You can’t have totality _and_ Turing-completeness, can you? Oh, but [you can](https://pdfs.semanticscholar.org/e291/5b546b9039a8cf8f28e0b814f6502630239f.pdf) – there is a particular type, `Partial a` (encoded with a fixed-point) that handles potential non-termination, akin to the way that `Maybe a` handles exceptional cases. It can be folded into `IO` in your main function, so that the runtime can execute a Turing-complete program that was modeled totally.

## organization

- [`yaya`](core/README.md) – the safe operations and data types for working with recursion
- [`yaya-hedgehog`](hedgehog/README.md) – utilities for testing your Yaya-using code with [Hedgehog](https://github.com/hedgehogqa/haskell-hedgehog)
- [`yaya-unsafe`](unsafe/README.md) – unsafe instances and operations

## versioning

In the absolute, almost every change is a breaking change. This section describes how we mitigate that to provide minor updates and revisions compatible with [SemVer 2.0.0](https://semver.org/spec/v2.0.0.html).

Here are some common changes that can have unintended effects:

- adding instances can conflict with downstream orphans,
- adding a module can conflict with a module from another package,
- adding a definition to an existing module can conflict if there are unqualified imports, and
- even small bugfixes can introduce breaking changes where downstream depended on the broken results.

To mitigate some of those issues for versioning, we assume the following usage:

- modules should be imported using `PackageImports`, so that adding modules is a _minor_ change;
- modules should be imported qualified, so that adding definitions is a _minor_ change;
- adding instances can't be mitigated in the same way, and it's not uncommon for downstream libraries to add orphans instances when they're omitted from upstream libraries. However, since these conflicts can only happen via direct dependencies, and represent an explicit downstream workaround, it’s reasonable to expect a quick downstream update to remove or conditionalize the workaround. So, this is considered a _minor major_ change;
- deprecation is considered a _revision_ change, however it will often be paired with _minor_ changes. `-Werror` can cause this to fail, but published libraries shouldn't be compiled with `-Werror`.

## building & development

Especially if you are unfamiliar with the Haskell ecosystem, there is a Nix build (both with and without a flake). If you are unfamiliar with Nix, [Nix adjacent](...) can help you get things working in the shortest time and least effort possible.

### if you have `nix` installed

`nix build` will build and test the project fully.

`nix develop` will put you into an environment where the traditional build tooling works. If you also have `direnv` installed, then you should automatically be in that environment when you're in a directory in this project.

### traditional build

This project is built with [Cabal](https://cabal.readthedocs.io/en/stable/index.html). Individual packages will work with older versions, but ./cabal.package requires Cabal 3.6+.

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
