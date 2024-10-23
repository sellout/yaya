# Yaya

[![built with garnix](https://img.shields.io/endpoint?url=https%3A%2F%2Fgarnix.io%2Fapi%2Fbadges%2Fsellout%2Fyaya)](https://garnix.io/repo/sellout/yaya)

Yet another … yet another recursion scheme library for Haskell

Total recursion scheme library for Haskell.

## overview

Recursion schemes allow you to separate _any_ recursion from your business logic, writing step-wise operations that can be applied in a way that guarantees termination (or, dually, progress).

How's this possible? You can’t have totality _and_ Turing-completeness, can you? Oh, but [you can](https://pdfs.semanticscholar.org/e291/5b546b9039a8cf8f28e0b814f6502630239f.pdf) – there is a particular type, `Partial a` (encoded with a fixed-point) that handles potential non-termination, akin to the way that `Maybe a` handles exceptional cases. It can be folded into `IO` in your main function, so that the runtime can execute a Turing-complete program that was modeled totally.

## organization

- [`yaya`](core/README.md) – the safe operations and data types for working with recursion
- [`yaya-containers`](./containers/README.md) – pattern functors and instances for the [containers](https://hackage.haskell.org/package/containers) package
- [`yaya-hedgehog`](hedgehog/README.md) – utilities for testing your Yaya-using code with [Hedgehog](https://github.com/hedgehogqa/haskell-hedgehog#readme)
- [`yaya-quickcheck`](quickcheck/README.md) – utilities for testing your Yaya-using code with [QuickCheck](https://github.com/nick8325/quickcheck#readme)
- [`yaya-unsafe`](unsafe/README.md) – unsafe instances and operations

## usage

See [the package README](./yaya/README.md) for usage information.

## building

Especially if you are unfamiliar with the Haskell ecosystem, there is a Nix build (both with and without a flake). If you are unfamiliar with Nix, [Nix adjacent](...) can help you get things working in the shortest time and least effort possible.

### if you have `nix` installed

`nix build` will build and test the project fully.

`nix develop` will put you into an environment where the traditional build tooling works. If you also have `direnv` installed, then you should automatically be in that environment when you're in a directory in this project.

### traditional build

This project is built with [Cabal](https://cabal.readthedocs.io/en/stable/index.html). Individual packages will work with older versions, but ./cabal.package requires Cabal 3.6+.

## development

### environment

We recommend the following steps to make working in this repository as easy as possible.

#### `direnv allow`

This command ensures that any work you do within this repository is done within a consistent reproducible environment. That environment provides various debugging tools, etc. When you leave this directory, you will leave that environment behind, so it doesn’t impact anything else on your system.

#### `git config --local include.path ../.config/git/config`

This will apply our repository-specific Git configuration to `git` commands run against this repository. It’s lightweight (you should definitely look at it before applying this command) – it does things like telling `git blame` to ignore formatting-only commits.

### CI failures

There are a few jobs that may fail during CI and indicate specific changes that need to be made to your PR. If you run into any failures other than those that are listed here, they likely have remedies that are specific to your changes. If you need help replicating or resolving them, or think that they represent general patterns like the ones listed below, inform the maintainers. They can help you resolve them and decide if they should be called out with generic resolution processes.

#### CI / check-bounds (check if bounds have changed)

A failure in the “check if bounds have changed” step indicates that the bounds on direct dependencies have changed.

It currently means that the discovered bounds have been restricted, which is always a breaking change. Unfortunately, this is sometimes not due to anything in the PR, but it does indicate we’re no longer testing the versions we used to – the Cabal solver will sometimes start choosing different packages, depending on releases. Due to the behavior of the solver, the most likely ones to change are in the middle of the range. There are a few ways to address this problem:

1. Simply change the bounds as the output recommends, and make sure the PR bumps the major version number. If this change is already bumping the major version, this is probably the right choice to make.
2. Try to force Cabal to try the previous bounds. If you had manually changed the bounds because you needed some new feature, is it possible to conditionalize use of that feature so that we can also still use and test with older bounds?
3. Tell CI that you want to keep the bounds the same even though they’re not tested. You do this by adding the old bound to the `extraDependencyVersions` list in flake.nix. This should be done carefully, but one use case is where those bounds _are_ tested by the Nix builds, but not by GitHub.

#### CI / check-licenses (check if licenses have changed)

This means there has _possibly_ been some change in the licensing, but it’s not foolproof. This only captures the licensing for one particular Cabal solution, so other solutions may have different transitive dependencies or licenses.

If there is a new license type in the list, it could affect how consumers of this can use our library. If the new license isn’t compatible with the existing set, then that’s a breaking change. If a package has changed its license, then we can alternatively restrict that package to versions that only use the previous license. Since making a license more restrictive introduces incompatibilities, this should only happen when they bump their major version, but there is no guarantee. In that case, this should just prevent us from extending the bounds, which is fine. But if it requires restricting bounds at the minor or revision level, then that’s still a breaking change on our side. Ideally we wouldn’t have to restrict that, but just make sure the consumer is informed about the license change and how to avoid it, but I don’t know how to convey that yet.

If there is a new dependency that has appeared, that should already be reflected in a major version bump. However, not all libraries introduce a major version bump when they add a dependency, and supporting wider version ranges means we may pick up a new dependency without excluding solutions that don’t involve that dependency.

It’s tempting to think that moving a dependency from the transitive list to the direct list doesn’t involve a version bump, but that’s not necessarily true. First, the transitive dependency must exist on all possible dependency solutions for that to be true. Then, it’s also possible for a new revision of a library to _remove_ dependencies, which means they will no longer appear in the transitive graph, invalidating our previous assumption. For this reason, we shouldn’t treat a move from transitive to direct as any different from a new dependency.

#### check formatter

There is some unformatted code (or perhaps some lint that needs addressing). If you use Nix, running `nix fmt` should automatically fix most of the formatting, and at least report additional lint that needs addressing.

If you don’t use Nix, the CI log should contain some lines like

```
treefmt 0.6.1
[INFO ] #alejandra: 1 files processed in 43.00ms
[INFO ] #prettier: 7 files processed in 423.85ms
[INFO ] #ormolu: 39 files processed in 1.60s
[INFO ] #hlint: 39 files processed in 2.15s
0 files changed in 2s (found 66, matched 86, cache misses 86)
```

Those `INFO` lines indicate which formatters were run. Running those same ones individually should address the issues. You can also just indicate in your PR that you don’t use Nix, and a maintainer will happily fix the formatting for you.

This implies a revision bump in any package that has been reformatted, as well as a revision bump in the repository.

#### check project-manager-files

Some files committed to the repository don’t match the ones that would be generated by Project Manager. This can happen either because you modified some of the Nix project configuration and forgot to regenerate the files, or because you edited generated files directly rather than editing the Nix project configuration.

If you use Nix, running `project-manager switch` from a project `devShell` (or `nix run github:sellout/project-manager -- switch`) anywhere should fix this (although check to see if you lost intentional changes to generated files, and add them via the Nix project configuration instead).

If you don’t use Nix, you will need to mention that in your PR so that one of the maintainers can resolve this for you.

## comparisons

See [the package README](./core/README.md) for comparisons with other similar projects.
