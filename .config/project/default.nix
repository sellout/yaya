{
  config,
  flaky,
  lib,
  pkgs,
  self,
  supportedSystems,
  ...
}: let
  githubSystems = [
    "macos-13" # x86_64-darwin
    "macos-14" # aarch64-darwin
    "ubuntu-22.04" # x86_64-linux
    "windows-2022"
  ];
in {
  project = {
    name = "yaya";
    summary = "Yet another … yet another recursion scheme library for Haskell";

    devPackages = [
      pkgs.cabal-install
      pkgs.graphviz
      ## So cabal-plan(-bounds) can be built in a devShell, since it doesn’t
      ## work in Nix proper.
      pkgs.zlib
    ];
  };

  imports = [
    (import ./github-ci.nix {
      systems = githubSystems;
      packages = {
        yaya = "core";
        yaya-containers = "containers";
        yaya-hedgehog = "hedgehog";
        yaya-quickcheck = "quickcheck";
        yaya-unsafe = "unsafe";
      };
      exclude = [
        ## TODO: This particular combination requires libgmp.
        {
          bounds = "--prefer-oldest";
          ghc = "8.6.1";
          os = "macos-13";
        }
        ## TODO: This build often hangs for some reason.
        {
          bounds = "--prefer-oldest";
          ghc = "8.8.1";
          os = "windows-2022";
        }
        ## TODO: For some reason, this combination fails to build
        ## ghc-paths-0.1.0.12.
        {
          ghc = "9.4.1";
          os = "macos-14";
        }
      ]
      ## GitHub can’t install GHC older than 9.4 on macos-14.
      ++ map (ghc: {
        inherit ghc;
        os = "macos-14";
      }) (builtins.filter (ghc: lib.versionOlder ghc "9.4")
        self.lib.nonNixTestedGhcVersions);
    })
    ./hackage-publish.nix
    ./hlint.nix
  ];

  ## dependency management
  services.renovate.enable = true;

  ## development
  programs = {
    direnv.enable = true;
    # This should default by whether there is a .git file/dir (and whether it’s
    # a file (worktree) or dir determines other things – like where hooks
    # are installed.
    git = {
      enable = true;
      ignores = [
        # Cabal build
        "dist-newstyle"
      ];
    };
  };

  ## formatting
  editorconfig.enable = true;

  programs = {
    treefmt = {
      enable = true;
      ## Haskell formatter
      programs.ormolu.enable = true;
      settings.formatter.prettier.excludes = ["*/docs/license-report.md"];
    };
    vale = {
      enable = true;
      excludes = [
        "*.cabal"
        "*.hs"
        "*.lhs"
        "./cabal.project"
      ];
      formatSettings."*"."Microsoft.Auto" = "NO";
      vocab.${config.project.name}.accept = [
        "bugfix"
        "coalgebra"
        "coinductive"
        "comonad"
        "compdata"
        "conditionalize"
        "Droste"
        "effectful"
        "Elgot"
        "functor"
        "GADT"
        "Kleisli"
        "Kmett"
        "metamorphism"
        "parameterize"
        "polykind"
        "[Yy]aya"
      ];
    };
  };

  ## CI
  services.garnix = {
    enable = true;
    builds = {
      ## TODO: Remove once garnix-io/garnix#285 is fixed.
      exclude = ["homeConfigurations.x86_64-darwin-example"];
      include = lib.mkForce (
        [
          "homeConfigurations.*"
          "nixosConfigurations.*"
        ]
        ++ flaky.lib.forGarnixSystems supportedSystems (
          sys:
            [
              "checks.${sys}.*"
              "devShells.${sys}.default"
              "packages.${sys}.default"
            ]
            ++ lib.concatMap (ghc: [
              "devShells.${sys}.${ghc}"
              "packages.${sys}.${ghc}_all"
            ])
            (self.lib.testedGhcVersions sys)
        )
      );
    };
  };
  ## FIXME: Shouldn’t need `mkForce` here (or to duplicate the base contexts).
  ##        Need to improve module merging.
  services.github.settings.branches.main.protection.required_status_checks.contexts =
    lib.mkForce
    (["check-bounds"]
      ++ lib.concatMap (sys:
        lib.concatMap (ghc: [
          "build (${ghc}, ${sys})"
          "build (--prefer-oldest, ${ghc}, ${sys})"
        ])
        self.lib.nonNixTestedGhcVersions)
      githubSystems
      ++ flaky.lib.forGarnixSystems supportedSystems (sys:
        lib.concatMap (ghc: [
          "devShell ${ghc} [${sys}]"
          "package ${ghc}_all [${sys}]"
        ])
        (self.lib.testedGhcVersions sys)
        ++ [
          "homeConfig ${sys}-${config.project.name}-example"
          "package default [${sys}]"
          ## FIXME: These are duplicated from the base config
          "check formatter [${sys}]"
          "check project-manager-files [${sys}]"
          "check vale [${sys}]"
          "devShell default [${sys}]"
        ]));

  ## publishing
  # NB: Can’t use IFD on FlakeHub (see DeterminateSystems/flakehub-push#69), so
  #     this is disabled until we have a way to build Haskell without IFD.
  services.flakehub.enable = false;
  services.github.enable = true;
  services.github.settings.repository.topics = ["recursion-schemes"];
}
