with import ./nixpkgs.nix {};

mkShell {
  buildInputs = [
    (haskellPackages.ghcWithPackages (p: with p; [
      stack
    ]))
  ];
}
