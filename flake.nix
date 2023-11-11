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
    ## TODO: Some checks currently don't work when sandboxed.
    sandbox = false;
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
  outputs = inputs: let
    pname = "yaya";

    ## Test the oldest revision possible for each minor release. If it’s not
    ## available in nixpkgs, test the oldest available, then try an older one
    ## via GitHub workflow. Additionally, check any revisions that have explicit
    ## conditionalization.
    supportedGhcVersions = [
      # "ghc884" # dependency compiler-rt-libc is broken in nixpkgs 23.05
      "ghc8107"
      "ghc902"
      "ghc928"
      "ghc945"
      "ghc961"
      # "ghcHEAD" # doctest doesn’t work on current HEAD
    ];

    supportedSystems = inputs.flake-utils.lib.defaultSystems;

    cabalPackages = pkgs: hpkgs: let
      packages =
        inputs.concat.lib.cabalProject2nix
        ./cabal.project
        pkgs
        hpkgs
        (old: {
          configureFlags = old.configureFlags ++ ["--ghc-options=-Werror"];
        });
    in
      packages
      // {
        yaya-test = inputs.self.lib.testOnly packages.yaya-test;
        yaya-unsafe-test = inputs.self.lib.testOnly packages.yaya-unsafe-test;
      };
  in
    {
      schemas = {
        inherit
          (inputs.flaky.schemas)
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
          inputs.concat.lib.overlayHaskellPackages
          supportedGhcVersions
          inputs.self.overlays.haskell;

        haskell = final: prev: hfinal: hprev:
          inputs.concat.lib.haskellOverlay cabalPackages final prev hfinal hprev
          // {
            ## hls-*-plugin tests fail with GHC 9.4.5 on x86_64-linux..
            hls-cabal-plugin =
              ## TODO: Only disable  checks for "ghc945".
              if final.system == inputs.flake-utils.lib.system.x86_64-linux
              then final.haskell.lib.dontCheck hprev.hls-cabal-plugin
              else hprev.hls-cabal-plugin;
            hls-floskell-plugin =
              ## TODO: Only disable  checks for "ghc945".
              if final.system == inputs.flake-utils.lib.system.x86_64-linux
              then final.haskell.lib.dontCheck hprev.hls-floskell-plugin
              else hprev.hls-floskell-plugin;
          };
      };

      homeConfigurations =
        builtins.listToAttrs
        (builtins.map
          (inputs.flaky.lib.homeConfigurations.example
            pname
            inputs.self
            [
              ({pkgs, ...}: {
                home.packages = [
                  (pkgs.haskellPackages.ghcWithPackages (hpkgs: [
                    hpkgs.${pname}
                    hpkgs.yaya-hedgehog
                    hpkgs.yaya-unsafe
                  ]))
                ];
              })
            ])
          supportedSystems);

      ## TODO: Move upstream.
      lib = {
        ## Don’t install this Haskell package – it only contains test suites.
        testOnly = drv:
          drv.overrideAttrs (old: {
            installPhase = ''
              runHook preInstall
              mkdir -p "$out"
              runHook postInstall
            '';
            outputs = ["out"];
          });
      };
    }
    // inputs.flake-utils.lib.eachSystem supportedSystems
    (system: let
      pkgs = import inputs.nixpkgs {
        inherit system;
        ## NB: This uses `inputs.self.overlays.default` because packages need to
        ##     be able to find other packages in this flake as dependencies.
        overlays = [inputs.self.overlays.default];
      };

      ## TODO: Extract this automatically from `pkgs.haskellPackages`.
      defaultCompiler = "ghc928";
    in {
      packages =
        {default = inputs.self.packages.${system}."${defaultCompiler}_all";}
        // inputs.concat.lib.mkPackages pkgs supportedGhcVersions cabalPackages;

      devShells =
        {default = inputs.self.devShells.${system}.${defaultCompiler};}
        // inputs.concat.lib.mkDevShells
        pkgs
        supportedGhcVersions
        cabalPackages
        (hpkgs: [
          hpkgs.haskell-language-server
          pkgs.cabal-install
          pkgs.graphviz
        ]);

      projectConfigurations = inputs.flaky.lib.projectConfigurations.default {
        inherit pkgs;
        inherit (inputs) self;
      };

      checks = inputs.self.projectConfigurations.${system}.checks;
      formatter = inputs.self.projectConfigurations.${system}.formatter;
    });

  inputs = {
    # Currently contains our Haskell/Nix lib that should be extracted into its
    # own flake.
    concat = {
      inputs = {
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

    nixpkgs.url = "github:NixOS/nixpkgs/release-23.05";
  };
}
