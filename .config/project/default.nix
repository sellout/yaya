{config, flaky, lib, ...}: {
  project = {
    name = "yaya";
    summary = "Yet another … yet another recursion scheme library for Haskell";
  };

  imports = [
    ./github-ci.nix
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
    };
    vale = {
      enable = true;
      excludes = [
        "*.cabal"
        "*.hs"
        "*.lhs"
        "./.gitattributes"
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
    builds.exclude = [
      # TODO: Remove once garnix-io/garnix#285 is fixed.
      "homeConfigurations.x86_64-darwin-${config.project.name}-example"
    ];
  };
  ## FIXME: Shouldn’t need `mkForce` here (or to duplicate the base contexts).
  ##        Need to improve module merging.
  services.github.settings.branches.main.protection.required_status_checks.contexts =
    lib.mkForce
      (map (ghc: "CI / build (${ghc}) (pull_request)") [
        "8.6.1"
        "8.8.1"
        "8.10.1"
        "9.0.1"
        "9.2.1"
        "9.4.1"
      ]
      ++ lib.concatMap flaky.lib.garnixChecks (
        lib.concatMap (ghc: [
          (sys: "devShell ghc${ghc} [${sys}]")
          (sys: "package ghc${sys}_all [${sys}]")
        ])
        ["8107" "902" "928" "945" "961"]
        ++ [
          (sys: "homeConfig ${sys}-${config.project.name}-example")
          (sys: "package default [${sys}]")
          ## FIXME: These are duplicated from the base config
          (sys: "check formatter [${sys}]")
          (sys: "devShell default [${sys}]")
        ]));

  ## publishing
  services.flakehub.enable = true;
  services.github.enable = true;
}
