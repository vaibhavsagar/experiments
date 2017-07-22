import ../ihaskell-nix {
  packages = self: with self; [
    attoparsec
    byteable
    bytestring
    zlib
    base16-bytestring
    containers
    SHA
  ];
  systemPackages = pkgs: with pkgs; [
    coreutils
    git
    qpdf
  ];
}
