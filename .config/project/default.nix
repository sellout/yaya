{
  config,
  flaky,
  lib,
  pkgs,
  self,
  supportedSystems,
  ...
}: {
  project = {
    name = "yaya";
    summary = "Yet another … yet another recursion scheme library for Haskell";
  };

  imports = [./hlint.nix];

  ## dependency management
  services.renovate.enable = true;

  ## development
  programs = {
    direnv.enable = true;
    # This should default by whether there is a .git file/dir (and whether it’s
    # a file (worktree) or dir determines other things – like where hooks
    # are installed.
    git.enable = true;
  };

  ## formatting
  editorconfig.enable = true;

  programs = {
    treefmt.enable = true;
    vale.enable = true;
  };

  ## CI
  services.garnix.enable = true;
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
        lib.concatMap (ghc: [
          "build (${ghc}, ${sys})"
          "build (--prefer-oldest, ${ghc}, ${sys})"
        ])
        self.lib.nonNixTestedGhcVersions)
      self.lib.githubSystems);
  services.haskell-ci = {
    inherit (self.lib) defaultGhcVersion extraDependencyVersions;
    systems = self.lib.githubSystems;
    ghcVersions = self.lib.nonNixTestedGhcVersions;
    cabalPackages = {
      yaya = "core";
      yaya-containers = "containers";
      yaya-hedgehog = "hedgehog";
      yaya-quickcheck = "quickcheck";
      yaya-unsafe = "unsafe";
    };
    latestGhcVersion = "9.10.1";
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
    ];
  };

  ## publishing
  services.github.enable = true;
  services.github.settings.repository.topics = ["recursion-schemes"];
}
