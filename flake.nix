{
  description = "Yet another … yet another recursion scheme library for Haskell";

  nixConfig = {
    ## https://github.com/NixOS/rfcs/blob/master/rfcs/0045-deprecate-url-syntax.md
    extra-experimental-features = ["no-url-literals"];
    extra-substituters = ["https://cache.garnix.io"];
    extra-trusted-public-keys = [
      "cache.garnix.io:CTFPyKSLcx5RMJKfLo5EEPUObbA78b0YQ2DTCJXqr9g="
    ];
    ## Isolate the build.
    registries = false;
    sandbox = "relaxed";
  };

  ### This is a complicated flake. Here’s the rundown:
  ###
  ### overlays.default – includes all of the packages from cabal.project
  ### packages = {
  ###   default = points to `packages.${defaultGhcVersion}`
  ###   <ghcVersion>-<cabal-package> = an individual package compiled for one
  ###                                  GHC version
  ###   <ghcVersion>-all = all of the packages in cabal.project compiled for one
  ###                      GHC version
  ### };
  ### devShells = {
  ###   default = points to `devShells.${defaultGhcVersion}`
  ###   <ghcVersion> = a shell providing all of the dependencies for all
  ###                  packages in cabal.project compiled for one GHC version
  ### };
  ### checks.format = verify that code matches Ormolu expectations
  outputs = {
    concat,
    flake-utils,
    flaky,
    nixpkgs,
    self,
  }: let
    pname = "yaya";

    supportedSystems = flaky.lib.defaultSystems;

    cabalPackages = pkgs: hpkgs:
      concat.lib.cabalProject2nix
      ./cabal.project
      pkgs
      hpkgs
      (old: {
        configureFlags = old.configureFlags ++ ["--ghc-options=-Werror"];
      });
  in
    {
      schemas = {
        inherit
          (flaky.schemas)
          overlays
          homeConfigurations
          packages
          devShells
          projectConfigurations
          checks
          formatter
          ;
      };

      # see these issues and discussions:
      # - NixOS/nixpkgs#16394
      # - NixOS/nixpkgs#25887
      # - NixOS/nixpkgs#26561
      # - https://discourse.nixos.org/t/nix-haskell-development-2020/6170
      overlays = {
        default =
          concat.lib.overlayHaskellPackages
          (self.lib.supportedGhcVersions "")
          (final: prev:
            nixpkgs.lib.composeManyExtensions [
              ## TODO: I think this overlay is only needed by formatters,
              ##       devShells, etc., so it shouldn’t be included in the
              ##       standard overlay.
              (flaky.overlays.haskell-dependencies final prev)
              (self.overlays.haskell final prev)
              (self.overlays.haskellDependencies final prev)
            ]);

        haskell = concat.lib.haskellOverlay cabalPackages;

        haskellDependencies = final: prev: hfinal: hprev:
          (
            if nixpkgs.lib.versionAtLeast hprev.ghc.version "9.8.0"
            then let
              hspecVersion = "2_11_7";
            in {
              ## The default versions in Nixpkgs 23.11 don’t support GHC 9.8.
              doctest = hfinal.doctest_0_22_2;
              hedgehog = hfinal."hedgehog_1_4";
              hspec = hfinal."hspec_${hspecVersion}";
              hspec-core = hfinal."hspec-core_${hspecVersion}";
              hspec-discover = hfinal."hspec-discover_${hspecVersion}";
              hspec-meta = hfinal."hspec-meta_${hspecVersion}";
              semigroupoids = hfinal.semigroupoids_6_0_0_1;
              tagged = hfinal.tagged_0_8_8;
              th-abstraction = hfinal.th-abstraction_0_6_0_0;
              ## `Control.Monad.Trans.List` is gone with GHC 9.8, but
              ## `lifted-base` hasn’t updated its tests to avoid it.
              lifted-base = final.haskell.lib.dontCheck hprev.lifted-base;
              ## The default versions in Nixpkgs 23.11 don’t support
              ## th-abstraction 0.6.
              aeson = final.haskell.lib.doJailbreak hprev.aeson;
              bifunctors = hfinal.bifunctors_5_6_1;
              free = hfinal.free_5_2;
            }
            else if nixpkgs.lib.versionAtLeast hprev.ghc.version "8.10.0"
            then {}
            else
              {
                ## NB: Fails a single test case under GHC 8.8.4.
                doctest = final.haskell.lib.dontCheck hprev.doctest;
                ## NB: Tests fail to build under GHC 8.8.4.
                vector = final.haskell.lib.dontCheck hprev.vector;
              }
              // (
                if final.system == "i686-linux"
                then {
                  ## NB: Fails `prop_double_assoc` under GHC 8.8.4 on i686-linux.
                  QuickCheck = final.haskell.lib.dontCheck hprev.QuickCheck;
                }
                else {}
              )
          )
          // (
            if final.system == "i686-linux"
            then {
              enummapset = final.haskell.lib.dontCheck hprev.enummapset;
              sqlite-simple = final.haskell.lib.dontCheck hprev.sqlite-simple;
            }
            else {}
          );
      };

      homeConfigurations =
        builtins.listToAttrs
        (builtins.map
          (flaky.lib.homeConfigurations.example
            pname
            self
            [
              ({pkgs, ...}: {
                home.packages = [
                  (pkgs.haskellPackages.ghcWithPackages (hpkgs: [
                    hpkgs.${pname}
                    hpkgs."${pname}-containers"
                    hpkgs."${pname}-hedgehog"
                    hpkgs."${pname}-quickcheck"
                    hpkgs."${pname}-unsafe"
                  ]))
                ];
              })
            ])
          supportedSystems);

      lib = {
        ## TODO: Extract this automatically from `pkgs.haskellPackages`.
        defaultCompiler = "ghc948";

        ## Test the oldest revision possible for each minor release. If it’s not
        ## available in nixpkgs, test the oldest available, then try an older
        ## one via GitHub workflow. Additionally, check any revisions that have
        ## explicit conditionalization. And check whatever version `pkgs.ghc`
        ## maps to in the nixpkgs we depend on.
        testedGhcVersions = system:
          [
            self.lib.defaultCompiler
            "ghc981"
            # "ghcHEAD" # doctest doesn’t work on current HEAD
          ]
          ## dependency compiler-rt-libc-7.1.0 is broken in on aarch64-darwin.
          ++ nixpkgs.lib.optional (system != "aarch64-darwin") "ghc884";

        ## The versions that are older than those supported by Nix that we
        ## prefer to test against.
        nonNixTestedGhcVersions = [
          # `(->)` isn’t a type constructor before GHC 8.6.
          "8.6.1"
          "8.8.1"
          "8.10.1"
          "9.0.1"
          "9.2.1"
          "9.4.1"
          "9.6.1"
        ];

        ## However, provide packages in the default overlay for _every_
        ## supported version.
        supportedGhcVersions = system:
          self.lib.testedGhcVersions system
          ++ [
            "ghc8107"
            "ghc902"
            "ghc924"
            "ghc925"
            "ghc926"
            "ghc927"
            "ghc928"
            "ghc942"
            "ghc943"
            "ghc944"
            "ghc945"
            "ghc946"
            "ghc947"
            "ghc962"
            "ghc963"
          ];
      };
    }
    // flake-utils.lib.eachSystem supportedSystems
    (system: let
      pkgs = import nixpkgs {
        inherit system;
        ## NB: This uses `self.overlays.default` because packages need to
        ##     be able to find other packages in this flake as dependencies.
        overlays = [self.overlays.default];
      };
    in {
      packages =
        {default = self.packages.${system}."${self.lib.defaultCompiler}_all";}
        // concat.lib.mkPackages
        pkgs
        (self.lib.supportedGhcVersions system)
        cabalPackages;

      projectConfigurations =
        flaky.lib.projectConfigurations.default {inherit pkgs self;};

      devShells =
        {default = self.devShells.${system}.${self.lib.defaultCompiler};}
        // concat.lib.mkDevShells
        pkgs
        (self.lib.supportedGhcVersions system)
        cabalPackages
        (hpkgs:
          [self.projectConfigurations.${system}.packages.path]
          ## NB: Haskell Language Server no longer supports GHC <9.
          ## TODO: HLS also apparently broken on 9.8.1
          ++ nixpkgs.lib.optional
          (nixpkgs.lib.versionAtLeast hpkgs.ghc.version "9"
            && builtins.compareVersions hpkgs.ghc.version "9.8.1" != 0)
          hpkgs.haskell-language-server);

      checks = self.projectConfigurations.${system}.checks;
      formatter = self.projectConfigurations.${system}.formatter;
    });

  inputs = {
    # Currently contains our Haskell/Nix lib that should be extracted into its
    # own flake.
    concat = {
      inputs = {
        ## TODO: The version currently used by concat doesn’t support i686-linux.
        bash-strict-mode.follows = "flaky/bash-strict-mode";
        flake-utils.follows = "flake-utils";
        nixpkgs.follows = "nixpkgs";
      };
      url = "github:compiling-to-categories/concat";
    };

    flake-utils.url = "github:numtide/flake-utils";

    flaky = {
      inputs = {
        flake-utils.follows = "flake-utils";
        nixpkgs.follows = "nixpkgs";
      };
      url = "github:sellout/flaky";
    };

    nixpkgs.url = "github:NixOS/nixpkgs/release-23.11";
  };
}
