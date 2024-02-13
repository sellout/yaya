{lib, self, ...}: {
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
        matrix.os = ["macos-13" "ubuntu-22.04" "windows-2022"];
        ## TODO: Populate this as the difference between supported versions and
        ##       available nix package sets.
        matrix.ghc = self.lib.nonNixTestedGhcVersions;
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
            cabal-version = "3.10";
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
        {run = "cabal v2-test all $CONFIG";}
      ];
    };
  };
}
