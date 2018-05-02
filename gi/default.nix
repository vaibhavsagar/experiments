with import <nixpkgs> { overlays = [ (self: super: {
  all-cabal-hashes = self.fetchurl {
    url = "https://github.com/commercialhaskell/all-cabal-hashes/archive/5ed06084aa1933c8131bb674b5de86bbb977c5f0.tar.gz";
    sha256 = "06nva1c2wj7z8pagchlc5ax3rhb9mgc9myxh9k07p9vyi7s10zrk";
  };
}) ]; };
(haskellPackages.extend (self: super: {
  haskell-gi = self.callHackage "haskell-gi" "0.21.2" {};
  haskell-gi-base = haskell.lib.addBuildDepend (self.callHackage "haskell-gi-base" "0.21.1" {}) pkgs.gobjectIntrospection;
  haskell-gi-overloading = haskell.lib.dontHaddock (self.callHackage "haskell-gi-overloading" "0.0" {});
})).gi-pango
