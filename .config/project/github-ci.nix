{lib, pkgs, self, ...}: {
  services.github.workflow."build.yml".text = lib.generators.toYAML {} {
    name = "CI";
    on = {
      push.branches = ["main"];
      pull_request.types = [
        "opened"
        "synchronize"
      ];
    };
    jobs.build = {
      strategy = {
        fail-fast = false;
        matrix = {
          ## TODO: Populate this as the difference between supported versions
          ##       and available nix package sets.
          ghc = self.lib.nonNixTestedGhcVersions;
          os = ["macos-13" "ubuntu-22.04" "windows-2022"];
        };
      };
      runs-on = "\${{ matrix.os }}";
      env.CONFIG = "--enable-tests --enable-benchmarks";
      steps = [
        {uses = "actions/checkout@v4";}
        {
          uses = "haskell-actions/setup@v2";
          id = "setup-haskell-cabal";
          "with" = {
            ghc-version = "\${{ matrix.ghc }}";
            cabal-version = pkgs.cabal-install.version;
          };
        }
        {run = "cabal v2-update";}
        {run = "cabal v2-freeze $CONFIG";}
        {
          uses = "actions/cache@v4";
          "with" = {
            path = ''
              ''${{ steps.setup-haskell-cabal.outputs.cabal-store }}
              dist-newstyle
            '';
            key = "\${{ runner.os }}-\${{ matrix.ghc }}-\${{ hashFiles('cabal.project.freeze') }}";
          };
        }
        ## NB: The `doctests` suites don’t seem to get built without explicitly
        ##     doing so before running the tests.
        {run = "cabal v2-build all $CONFIG";}
        {run = "cabal v2-test all $CONFIG";}
      ];
    };
  };
}
