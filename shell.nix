{ pkgs ? import <unstable> {} }:

with pkgs;

stdenv.mkDerivation {
  buildInputs = [stack
                 haskellPackages.alex
                 haskellPackages.happy
                 haskellPackages.weeder
                 libiconv];

  name = "yaya";
}
