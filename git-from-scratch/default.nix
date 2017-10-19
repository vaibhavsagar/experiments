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
    rev    = "c99239bca08d12bf98000961912b4c0ad52a8a7e";
    sha256 = "1d3hwaflsyb6vj2czj3jpaxvdmsr448sd0536lhaillvsm087y0g";
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
