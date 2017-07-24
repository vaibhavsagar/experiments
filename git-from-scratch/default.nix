let IHaskell = (import <nixpkgs> {}).fetchFromGitHub {
    owner  = "gibiansky";
    repo   = "IHaskell";
    rev    = "b1a13244228ac209d85c8d113fc73a2e616fd202";
    sha256 = "12jk24pnhf2cjljp5z3f1ziv8ykaz7p3cnm1jfn6g7lnb33iifbl";
  };
in import "${IHaskell}/release.nix" {
  packages = self: with self; [
    SHA
    attoparsec
    base16-bytestring
    byteable
    bytestring
    containers
    directory
    hspec
    hspec-expectations
    filepath
    process
    utf8-string
    zlib
  ];
  systemPackages = pkgs: with pkgs; [
    coreutils
    git
    qpdf
  ];
}
