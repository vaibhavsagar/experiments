let pkgs = import <nixpkgs> {}; in {
  build     = pkgs.callPackage <ihaskellSrc/release.nix> {};
  build-8_2 = pkgs.callPackage <ihaskellSrc/release-8.2.nix> {};
}
