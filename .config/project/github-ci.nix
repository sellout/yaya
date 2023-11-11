{lib, ...}: {
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
      runs-on = "ubuntu-latest";
      ## TODO: Populate this as the difference between supported versions and
      ##       available nix package sets.
      strategy.matrix.ghc = [
        "8.6.1"
        "8.8.1"
        "8.10.1"
        "9.0.1"
        "9.2.1"
        "9.4.1"
      ];
      env.CONFIG = "--enable-tests --enable-benchmarks";
      steps = [
        {uses = "actions/checkout@v2";}
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
          uses = "actions/cache@v2";
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
