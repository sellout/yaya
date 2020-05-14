/*
NIXPKGS version

Any archive of nixpkgs can be used.

The simplest update solution is to look at
http://github.com/NixOS/nixpkgs-channels and pick the latest commit for
nixpkgs-unstable. The archive can then be fetched at:

https://github.com/NixOS/nixpkgs-channels/archive/COMMIT_NUMBER.tar.gz;

and the control sum computed using `sha256`.
*/

let
  # nixpkgs-unstable 2020-05-08
  sha256 = "1m202ifx2iy06dqmgih84a1gzf1vj5kw17dqzv3sc2ikwz1d7rnp";
  rev = "cfe68f2b68b7a7c2e5347c496b6963af30d18d3f";
in
import (fetchTarball {
  inherit sha256;
  url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
})
