let
  pkgs = import <nixpkgs> {};
in pkgs.stdenv.mkDerivation {
  name = "mosml-2.10.1";
  src = builtins.fetchTarball {
    url = "https://github.com/kfl/mosml/archive/ee355b296f393a8a0bf9de9e8dbe8a5915c92ed6.tar.gz";
    sha256 = "1jiyvdm8bxbfz6l6m1svwi7md5gzp0y5mx4p1dldhd1vyddgvb8q";
  };
  sourceRoot = "source/src";
  postPatch = ''substituteInPlace Makefile.inc --replace '/usr/local' "$out"'';
  buildInputs = [ pkgs.perl pkgs.gmp ];
}
