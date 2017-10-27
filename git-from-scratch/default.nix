let
  fetch    = (import <nixpkgs> {}).fetchFromGitHub;
  IHaskell = fetch (builtins.fromJSON (builtins.readFile ./ihaskell.json));
  pinned   = fetch (builtins.fromJSON (builtins.readFile ./nixpkgs.json));
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
