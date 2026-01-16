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
              (self.lib.supportedGhcVersions final.system))
            (final: prev:
              nixpkgs.lib.composeManyExtensions [
                ## TODO: I think this overlay is only needed by formatters,
                ##       devShells, etc., so it shouldn’t be included in the
                ##       standard overlay.
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
      haskellDependencies = final: prev: hfinal: hprev: {};
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

      ## For testing, we build against the _oldest_ version of each minor GHC
      ## release. However, for general development, we want to use Nixpkgs
      ## default GHC to maximize the cache benefit, etc.
      ##
      ## FIXME: This should be the assert below, but currently we have dependencies on this that don’t allow us to make it system-dependent.
      defaultGhcVersion = "9.6.6";

      ## Test the oldest revision possible for each minor release. If it’s not
      ## available in nixpkgs, test the oldest available, then try an older
      ## one via GitHub workflow. Additionally, check any revisions that have
      ## explicit conditionalization. And check whatever version `pkgs.ghc`
      ## maps to in the nixpkgs we depend on.
      testedGhcVersions = system:
        assert self.lib.defaultGhcVersion == nixpkgs.legacyPackages.${system}.haskellPackages.ghc.version; [
          self.lib.defaultGhcVersion
          "8.10.7"
          "9.0.2"
          "9.2.5"
          "9.4.5"
          "9.6.3"
          "9.8.1"
          "9.10.1"
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
      ];

      ## However, provide packages in the default overlay for _every_
      ## supported version.
      supportedGhcVersions = system:
        self.lib.testedGhcVersions system
        ++ [
          "9.2.6"
          "9.2.7"
          "9.2.8"
          "9.4.6"
          "9.4.7"
          "9.4.8"
          "9.6.4"
          "9.6.5"
          "9.8.2"
        ];

      ## These are versions that we don’t build against, but that we want the
      ## Haskell packages to support anyway. Mostly, this is for local
      ## packages where `--perfer-oldest` and `--allow-newer` have no effect,
      ## so reasonable bounds need to be managed manually.
      ##
      ## These also currently need to be restricted so that they work for all
      ## packages that have them as dependencies. E.g., most packages require
      ## `yaya ^>= 0.5.0`, but `yaya-unsafe` requires `yaya ^>= 0.5.1`, so
      ## this must specify `yaya-0.5.1.0`, not `yaya-0.5.0.0`.
      extraDependencyVersions = [
        "yaya-0.5.1.0"
        "yaya-0.6.0.0"
        "yaya-hedgehog-0.2.1.0"
        "yaya-hedgehog-0.3.0.0"
        ## This is the version used by Nix, so we can’t have
        ## `cabal-plan-bounds` throw it out.
        "th-abstraction-0.5.0.0"
        ## This only solves at 0.7.1, but the Haskell didn’t change, so keep
        ## the previous bound.
        "th-abstraction-0.7.0.0"
      ];

      githubSystems = [
        "macos-13" # x86_64-darwin
        "macos-14" # aarch64-darwin
        "ubuntu-24.04" # x86_64-linux
        "windows-2022"
      ];
    };
  }
  // flake-utils.lib.eachSystem supportedSystems
  (system: let
    pkgs = nixpkgs.legacyPackages.${system}.appendOverlays [
      flaky.overlays.default
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
      // flaky-haskell.lib.mkDevShells
      pkgs
      (map self.lib.nixifyGhcVersion (self.lib.supportedGhcVersions system))
      cabalPackages
      (hpkgs:
        [self.projectConfigurations.${system}.packages.path]
        ## NB: Haskell Language Server no longer supports GHC <9.2, and 9.4
        ##     has an issue with it on i686-linux.
        ## TODO: Remove the restriction on GHC 9.10 once
        ##       https://github.com/NixOS/nixpkgs/commit/e87381d634cb1ddd2bd7e121c44fbc926a8c026a
        ##       finds its way into 24.05.
        ++ nixpkgs.lib.optional
        (
          (
            if system == "i686-linux"
            then nixpkgs.lib.versionAtLeast hpkgs.ghc.version "9.4"
            else nixpkgs.lib.versionAtLeast hpkgs.ghc.version "9.2"
          )
          && nixpkgs.lib.versionOlder hpkgs.ghc.version "9.10"
        )
        hpkgs.haskell-language-server);

    projectConfigurations =
      flaky.lib.projectConfigurations.haskell {inherit pkgs self;};

    checks = self.projectConfigurations.${system}.checks;
    formatter = self.projectConfigurations.${system}.formatter;
  })
