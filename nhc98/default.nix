let
  pkgs = import <nixpkgs> {};
in pkgs.pkgsi686Linux.stdenv.mkDerivation {
  name = "nhc98";
  src = builtins.fetchurl {
    url = "http://www.haskell.org/nhc98/nhc98src-1.22.tar.gz";
    sha256 = "0fkgxgsd2iqxvwcgnad1702kradwlbcal6rxdrgb22vd6dnc3i8l";
  };
}
