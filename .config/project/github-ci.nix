{
  lib,
  pkgs,
  self,
  ...
}: let
  planName = "plan-\${{ runner.os }}-\${{ matrix.ghc }}\${{ matrix.bounds }}";
in {
  services.github.workflow."build.yml".text = lib.generators.toYAML {} {
    name = "CI";
    on = {
      push.branches = ["main"];
      pull_request.types = [
        "opened"
        "synchronize"
      ];
    };
    jobs = {
      build = {
        strategy = {
          fail-fast = false;
          matrix = {
            ghc = self.lib.nonNixTestedGhcVersions;
            os = ["macos-13" "ubuntu-22.04" "windows-2022"];
            bounds = ["--prefer-oldest" ""];
            exclude = [
              ## This combination currently fails due to a libgmp issue.
              ## Hopefully, constraining based on the remaining cases will
              ## result in bounds that eliminate the problem.
              {
                ghc = "8.6.1";
                os = "macos-13";
                bounds = "--prefer-oldest";
              }
              ## These jobs are currently hanging at the end of the build.
              {
                ghc = "8.10.1";
                os = "windows-2022";
              }
            ];
          };
        };
        runs-on = "\${{ matrix.os }}";
        env.CONFIG = "--enable-tests --enable-benchmarks \${{ matrix.bounds }}";
        steps = [
          {uses = "actions/checkout@v4";}
          {
            ## TODO: Uses deprecated Node.js, see haskell-actions/setup#72
            uses = "haskell-actions/setup@v2";
            id = "setup-haskell-cabal";
            "with" = {
              ghc-version = "\${{ matrix.ghc }}";
              cabal-version = pkgs.cabal-install.version;
            };
          }
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
          ## NB: The `doctests` suites don’t seem to get built without
          ##     explicitly doing so before running the tests.
          {run = "cabal v2-build all $CONFIG";}
          {run = "cabal v2-test all $CONFIG";}
          {run = "mv dist-newstyle/cache/plan.json ${planName}.json";}
          {
            name = "Upload build plan as artifact";
            uses = "actions/upload-artifact@v4";
            "with" = {
              name = planName;
              path = "${planName}.json";
            };
          }
        ];
      };
      check-bounds = {
        runs-on = "ubuntu-22.04";
        needs = ["build"];
        steps = [
          {uses = "actions/checkout@v4";}
          {
            ## TODO: Uses deprecated Node.js, see haskell-actions/setup#72
            uses = "haskell-actions/setup@v2";
            id = "setup-haskell-cabal";
            "with" = {
              ## NB: `cabal-plan-bounds` doesn’t yet support GHC 9.8.
              ghc-version = "9.6.3";
              cabal-version = pkgs.cabal-install.version;
            };
          }
          {run = "cabal install cabal-plan-bounds";}
          {
            name = "Load plans";
            uses = "actions/download-artifact@v4";
            "with" = {
              path = "plans";
              pattern = "plan-*";
              merge-multiple = true;
            };
          }
          {run = "find plans/";}
          {run = "find . -name '*.cabal' -exec cabal-plan-bounds plans/*.json --cabal {} \\;";}
          {run = "git diff **/*.cabal";}
          {
            name = "Fail if .cabal file was changed";
            run = "git diff-files --quiet **/*.cabal || exit 1";
          }
        ];
      };
    };
  };
}
