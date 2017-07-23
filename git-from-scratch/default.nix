import ../ihaskell-nix {
  packages = self: with self; [
    SHA
    attoparsec
    base16-bytestring
    byteable
    bytestring
    containers
    filepath
    utf8-string
    zlib
  ];
  systemPackages = pkgs: with pkgs; [
    coreutils
    git
    qpdf
  ];
}
