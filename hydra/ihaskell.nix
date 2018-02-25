let pkgs = import <nixpkgs> {}; in {
  build     = pkgs.callPackage <ihaskellSrc/release.nix> {
    packages = self: with self; [
      ihaskell-aeson
      ihaskell-blaze
      ihaskell-charts
      ihaskell-diagrams
      ihaskell-gnuplot
      ihaskell-hatex
      ihaskell-juicypixels
      ihaskell-magic
      ihaskell-plot
      # ihaskell-rlangqq
      ihaskell-static-canvas
      # ihaskell-widgets
    ];
  };
  build-8_2 = pkgs.callPackage <ihaskellsrc/release-8.2.nix> {
    packages = self: with self; [
      ihaskell-aeson
      ihaskell-blaze
      ihaskell-charts
      ihaskell-diagrams
      ihaskell-gnuplot
      ihaskell-hatex
      ihaskell-juicypixels
      ihaskell-magic
      ihaskell-plot
      # ihaskell-rlangqq
      ihaskell-static-canvas
      # ihaskell-widgets
    ];
  };
  build-8_4 = pkgs.callPackage <ihaskellsrc/release-8.4.nix> {
    packages = self: with self; [
    ];
  };
}
