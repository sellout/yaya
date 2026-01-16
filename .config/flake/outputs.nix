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
{
  flake-utils,
  flaky,
  flaky-haskell,
  nixpkgs,
  self,
  systems,
}: let
  pname = "yaya";

  supportedSystems = import systems;

  cabalPackages = pkgs: hpkgs:
    flaky-haskell.lib.cabalProject2nix
    ../../cabal.project
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
      default = final:
        nixpkgs.lib.composeManyExtensions [
          flaky.overlays.default
          (flaky-haskell.lib.overlayHaskellPackages
            (map self.lib.nixifyGhcVersion
              (self.lib.supportedGhcVersions final.stdenv.hostPlatform.system))
            (final: prev:
              nixpkgs.lib.composeManyExtensions [
                (flaky.overlays.haskellDependencies final prev)
                (self.overlays.haskell final prev)
                (self.overlays.haskellDependencies final prev)
              ]))
        ]
        final;

      haskell = flaky-haskell.lib.haskellOverlay cabalPackages;

      ## NB: Dependencies that are overridden because they are broken in
      ##     Nixpkgs should be pushed upstream to Flaky. This is for
      ##     dependencies that we override for reasons local to the project.
      haskellDependencies = final: prev: hfinal: hprev:
        if final.stdenv.hostPlatform.system == "i686-linux"
        then
          if
            final.lib.versionAtLeast hprev.ghc.version "9.6"
            && final.lib.versionOlder hprev.ghc.version "9.8"
          then {
            retrie = final.haskell.lib.dontCheck hprev.retrie;
          }
          else if
            final.lib.versionAtLeast hprev.ghc.version "9.8"
            && final.lib.versionOlder hprev.ghc.version "9.10"
          then {
            bifunctors = hfinal.callHackageDirect {
              pkg = "bifunctors";
              ver = "5.6.1";
              sha256 = "PE+ymT2cUsEeTqgN5ty/BGqzvWlyj+fPJjYvMsbZYoo=";
              rev = {
                revision = "3";
                sha256 = "UCogGFWjMm433cfI5+yEImvWB/DrBvUCLN+iZxavx+0=";
              };
            } {};
            lens = final.haskell.lib.doJailbreak (hfinal.callHackageDirect {
              pkg = "lens";
              ver = "5.3.1"; # needs to be jailbroken for hashable
              sha256 = "4Yk/899ZhnZx6XKLbCS1zuURSF5YpujxWfMZmDbqwSs=";
            } {});
            os-string = final.haskell.lib.dontCheck hprev.os-string;
          }
          else {}
        else {};
    };

    homeConfigurations =
      builtins.listToAttrs
      (builtins.map
        (flaky.lib.homeConfigurations.example self [
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
      nixifyGhcVersion = version:
        "ghc" + nixpkgs.lib.replaceStrings ["."] [""] version;

      ## TODO: Extract this automatically from `pkgs.haskellPackages`.
      defaultGhcVersion = "9.10.3";

      ## Test the oldest revision possible for each minor release. If it’s not
      ## available in nixpkgs, test the oldest available, then try an older
      ## one via GitHub workflow. Additionally, check any revisions that have
      ## explicit conditionalization. And check whatever version `pkgs.ghc`
      ## maps to in the nixpkgs we depend on.
      testedGhcVersions = system: [
        self.lib.defaultGhcVersion
        "9.4.8"
        "9.6.7"
        "9.8.4"
        "9.10.2"
        "9.12.2"
        # "ghcHEAD" # doctest doesn’t work on current HEAD
      ];

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
        ## since `cabal-plan-bounds` doesn’t work under Nix
        "9.8.1"
        "9.10.1"
        "9.12.1"
        "9.14.1"
      ];

      ## However, provide packages in the default overlay for _every_
      ## supported version.
      supportedGhcVersions = self.lib.testedGhcVersions;
    };
  }
  // flake-utils.lib.eachSystem supportedSystems
  (system: let
    pkgs = nixpkgs.legacyPackages.${system}.appendOverlays [
      ## NB: This uses `self.overlays.default` because packages need to be
      ##     able to find other packages in this flake as dependencies.
      self.overlays.default
    ];
  in {
    packages =
      {
        default =
          self.packages.${system}."${self.lib.nixifyGhcVersion self.lib.defaultGhcVersion}_all";
      }
      // flaky-haskell.lib.mkPackages
      pkgs
      (map self.lib.nixifyGhcVersion (self.lib.supportedGhcVersions system))
      cabalPackages;

    devShells =
      {
        default =
          self.devShells.${system}.${self.lib.nixifyGhcVersion self.lib.defaultGhcVersion};
      }
      // self.projectConfigurations.${system}.devShells
      // flaky-haskell.lib.mkDevShells
      pkgs
      (map self.lib.nixifyGhcVersion (self.lib.supportedGhcVersions system))
      cabalPackages
      (hpkgs:
        [self.projectConfigurations.${system}.packages.path]
        ## NB: Haskell Language Server no longer supports GHC <9.6.
        ++ nixpkgs.lib.optional
        (nixpkgs.lib.versionAtLeast hpkgs.ghc.version "9.6"
          ## TODO: With Nixpkgs 25.11, HLS complains about conflicting package
          ##       versions in these combinations, so skip it.
          && !(pkgs.stdenv.hostPlatform.isLinux
            && nixpkgs.lib.versionAtLeast hpkgs.ghc.version "9.8"
            && nixpkgs.lib.versionOlder hpkgs.ghc.version "9.10"))
        hpkgs.haskell-language-server);

    projectConfigurations =
      flaky.lib.projectConfigurations.haskell {inherit pkgs self;};

    checks = self.projectConfigurations.${system}.checks;
    formatter = self.projectConfigurations.${system}.formatter;
  })
