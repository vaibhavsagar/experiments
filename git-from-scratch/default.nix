let
  fetch    = (import <nixpkgs> {}).fetchFromGitHub;
  IHaskell = fetch {
    owner  = "gibiansky";
    repo   = "IHaskell";
    rev    = "32aea170c41b181e34e76b9d9eb391e540fed6e1";
    sha256 = "17n24pxizifvsxyh4lc8jgr45csbmrz936qssj8i5l96yr2fqz4b";
  };
  pinned   = fetch {
    owner  = "NixOS";
    repo   = "nixpkgs";
    rev    = "81628ce54f703fd518432ec3b429083cb183d747";
    sha256 = "01bir37cxwi252977prxgd0nmlzybcs4x7w2an51jim6mvk7500s";
  };
in import "${IHaskell}/release.nix" {
  nixpkgs = import pinned {};
  packages = self: with self; [
    SHA
    attoparsec
    base16-bytestring
    byteable
    bytestring
    containers
    directory
    filepath
    utf8-string
    zlib
  ];
  systemPackages = pkgs: with pkgs; [
    coreutils
    findutils
    git
    qpdf
  ];
}
