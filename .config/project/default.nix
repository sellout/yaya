### All available options for this file are listed in
### https://sellout.github.io/project-manager/options.xhtml
{
  config,
  lib,
  self,
  ...
}: {
  project = {
    name = "yaya";
    summary = "Yet another … yet another recursion scheme library for Haskell";
    ## TODO: Move something like this to Flaky.
    file = let
      copyLicenses = dir: {
        "${dir}/LICENSE".source = ../../LICENSE;
        "${dir}/LICENSE.AGPL-3.0-only".source = ../../LICENSE.AGPL-3.0-only;
        "${dir}/LICENSE.Universal-FOSS-exception-1.0".source =
          ../../LICENSE.Universal-FOSS-exception-1.0;
        "${dir}/LICENSE.commercial".source = ../../LICENSE.commercial;
      };
    in
      copyLicenses "containers"
      // copyLicenses "core"
      // copyLicenses "hedgehog"
      // copyLicenses "quickcheck"
      // copyLicenses "unsafe";
  };

  imports = [./hlint.nix];

  ## CI
  ## FIXME: Shouldn’t need `mkForce` here (or to duplicate the base contexts).
  ##        Need to improve module merging.
  services.github.settings.branches.main.protection.required_status_checks.contexts =
    lib.mkForce
    ([
        "All Garnix checks"
        "check-bounds"
        "check-licenses"
      ]
      ++ lib.concatMap (sys:
        lib.concatMap (ghc:
          ## Don’t add `exclude`d matrix entries to the required list
          ##
          ## TODO: Make this less manual (like the `include` component).
            if
              builtins.elem ghc ["7.10.3" "8.0.2" "8.2.2"]
              && sys == "ubuntu-24.04"
              || lib.versionOlder ghc "9.2" && builtins.elem sys ["macos-15" "ubuntu-24.04-arm"]
              || builtins.elem ghc ["8.8.1" "8.10.1"] && builtins.elem sys ["macos-15-intel" "windows-2025"]
              || ghc == "9.2.1" && builtins.elem sys ["macos-15" "ubuntu-24.04-arm"]
              || ghc == "9.4.1" && builtins.elem sys ["macos-15" "windows-2025"]
            then []
            else [
              "build (${ghc}, ${sys})"
              "build (--prefer-oldest, ${ghc}, ${sys})"
            ])
        self.lib.nonNixTestedGhcVersions)
      config.services.haskell-ci.systems
      ## Add `include`d matrix entries to the required list.
      ++ map (
        entry:
          if entry.bounds == ""
          then "build (${entry.ghc}, ${entry.os})"
          else "build (${entry.bounds}, ${entry.ghc}, ${entry.os})"
      )
      config.services.haskell-ci.include);
  services.haskell-ci = {
    inherit (self.lib) defaultGhcVersion;
    ghcVersions = self.lib.nonNixTestedGhcVersions;
    cabalPackages = {
      yaya = "core";
      yaya-containers = "containers";
      yaya-hedgehog = "hedgehog";
      yaya-quickcheck = "quickcheck";
      yaya-unsafe = "unsafe";
    };
    exclude =
      ## TODO: “can't load framework: Security (not found)”
      ##       Maybe this is fixable with some dependency constraint.
      ##       E.g., doctest 0.16.3 vs 0.24.3.
      ## NB: 8.8.1 build not replaced with newer GHC, because it still fails.
      map (ghc: {
        inherit ghc;
        bounds = "";
        os = "macos-15-intel";
      }) ["8.8.1" "8.10.1"]
      ++ [
        ## TODO: “Failed to build Win32-2.14.2.1. […] during the configure step.”
        ## NB: Not replaced with newer GHC, because build is flaky.
        {
          ghc = "8.8.1";
          os = "windows-2025";
        }
        ## TODO:  “Failed to build yaya-0.6.2.3. […] terminated with exit code 11”
        {
          ghc = "8.10.1";
          os = "windows-2025";
        }
        ## TODO: “Failed to build ghc-paths-0.1.0.12. […] segfaulted”
        ##       I wonder if this could have the same cause as the Ubuntu one
        ##       below – an issue with NUMA on ARM around this version.
        {
          ghc = "9.2.1";
          os = "macos-15";
        }
        ## TODO:  “/usr/bin/ld: cannot find -lnuma: No such file or directory”
        {
          ghc = "9.2.1";
          os = "ubuntu-24.04-arm";
        }
        ## TODO: “Failed to build ghc-paths-0.1.0.12. […] segfaulted”
        ##       I wonder if this could have the same cause as the Ubuntu one
        ##       below – an issue with NUMA on ARM around this version.
        {
          ghc = "9.4.1";
          os = "macos-15";
        }
        ## TODO: “Failed to build Cabal-syntax-3.16.1.0”
        {
          ghc = "9.4.1";
          os = "windows-2025";
        }
      ];
    ## These just try to build with a newer GHC from the same major version.
    ##
    ## TODO: Currently this just pick the _never_ version instead of the oldest,
    ##       but there may be a middle ground for some of them.
    include =
      [
        {
          bounds = "";
          ghc = "8.10.7";
          os = "macos-15-intel";
        }
      ]
      ++ map (bounds: {
        inherit bounds;
        ghc = "8.10.7";
        os = "windows-2025";
      }) ["--prefer-oldest" ""]
      ++ lib.concatMap (ghc:
        map (bounds: {
          inherit bounds ghc;
          os = "macos-15";
        }) ["--prefer-oldest" ""]) ["9.2.8" "9.4.8"]
      ++ map (bounds: {
        inherit bounds;
        ghc = "9.2.8";
        os = "ubuntu-24.04-arm";
      }) ["--prefer-oldest" ""]
      ++ map (bounds: {
        inherit bounds;
        ghc = "9.4.8";
        os = "windows-2025";
      }) ["--prefer-oldest" ""];
    ## These are versions that we don’t build against, but that we want the
    ## Haskell packages to support anyway. Mostly, this is for local packages
    ## where `--perfer-oldest` and `--allow-newer` have no effect, so reasonable
    ## bounds need to be managed manually.
    ##
    ## These also currently need to be restricted so that they work for all
    ## packages that have them as dependencies. E.g., most packages require
    ## `yaya ^>= 0.5.0`, but `yaya-unsafe` requires `yaya ^>= 0.5.1`, so this
    ## must specify `yaya-0.5.1.0`, not `yaya-0.5.0.0`.
    extraDependencyVersions = [
      "QuickCheck-2.15.0.1" # Used by Nixpkgs 25.11’s haskellPackages
      "yaya-0.5.1.0"
      "yaya-0.6.0.0"
      "yaya-hedgehog-0.2.1.0"
      "yaya-hedgehog-0.3.0.0"
    ];
    latestGhcVersion = "9.10.1";
  };

  ## publishing
  services.github.settings.repository.topics = ["recursion-schemes"];
}
