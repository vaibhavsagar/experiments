let pkgs = import <nixpkgs> {}; in {
  build = pkgs.callPackage <ihaskellSrc/release.nix> {};
}
