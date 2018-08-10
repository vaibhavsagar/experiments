{ stdenv, fetchurl, gmp, perl }:
let
  version = "2.10.1";
in
stdenv.mkDerivation {
  name = "mosml-${version}";
  src = fetchurl {
    url = "https://github.com/kfl/mosml/archive/ver-${version}.tar.gz";
    sha256 = "13x7wj94p0inn84pzpj52dch5s9lznqrj287bd3nk3dqd0v3kmgy";
  };
  setSourceRoot = ''export sourceRoot="$(echo */src)"'';
  configurePhase = ''substituteInPlace Makefile.inc --replace '/usr/local' "$out"'';
  buildInputs = [ gmp perl ];
}
