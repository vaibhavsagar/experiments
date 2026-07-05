let
  pkgs = import <nixpkgs> {};
in
  pkgs.mkShell {
    buildInputs = [ (pkgs.haskellPackages.ghcWithPackages (p: [ p.tidal ])) pkgs.supercollider-with-sc3-plugins ];
  }
