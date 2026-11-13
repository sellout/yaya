{
  description = "Yet another … yet another recursion scheme library for Haskell";

  nixConfig = {
    ## NOTE: This is a consequence of the prevailing Haskell infrastructure.
    allow-import-from-derivation = true;
    extra-substituters = ["https://sellout.cachix.org"];
    extra-trusted-public-keys = [
      "sellout.cachix.org-1:v37cTpWBEycnYxSPAgSQ57Wiqd3wjljni2aC0Xry1DE="
    ];
    ## WAIT: This should be `"fatal"`, but NixOS/nixpkgs#544986. Also, cabal2nix
    ##       uses absolute paths.
    lint-absolute-path-literals = "ignore";
    lint-short-path-literals = "fatal";
    lint-url-literals = "fatal";
    ## Isolate the build.
    sandbox = true;
    use-registries = false;
  };

  ## The flake isn’t a Nix expression, so it’s clearer to keep `outputs` (which
  ## is) in a separate file.
  outputs = inputs: import .config/flake/outputs.nix inputs;

  inputs = {
    ## Flaky should generally be the source of truth for its inputs.
    flaky.url = "github:sellout/flaky";

    flake-utils.follows = "flaky/flake-utils";
    nixpkgs.follows = "flaky/nixpkgs";
    systems.follows = "flaky/systems";

    flaky-haskell = {
      inputs.flaky.follows = "flaky";
      url = "github:sellout/flaky-haskell";
    };
  };
}
