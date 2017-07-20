import ../ihaskell-nix {
  packages = self: with self; [
    attoparsec
    bytestring
    zlib
    base16-bytestring
    containers
  ];
  systemPackages = pkgs: with pkgs; [
    coreutils
    git
    qpdf
  ];
}
