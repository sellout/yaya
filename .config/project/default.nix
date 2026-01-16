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
        lib.concatMap (ghc: [
          "build (${ghc}, ${sys})"
          "build (--prefer-oldest, ${ghc}, ${sys})"
        ])
        self.lib.nonNixTestedGhcVersions)
      config.services.haskell-ci.systems);
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
