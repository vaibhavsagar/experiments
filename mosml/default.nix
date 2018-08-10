{ stdenv, fetchurl, gmp, perl }:
stdenv.mkDerivation {
  name = "mosml-2.10.1";
  src = fetchurl {
    url = "https://github.com/kfl/mosml/archive/ee355b296f393a8a0bf9de9e8dbe8a5915c92ed6.tar.gz";
    sha256 = "1dxjjaw0cnngfx55bd25f7nbqbhv7svxsji9f8gmc6sgkpyx9xhv";
  };
  setSourceRoot = ''export sourceRoot="$(echo */src)"'';
  configurePhase = ''substituteInPlace Makefile.inc --replace '/usr/local' "$out"'';
  buildInputs = [ gmp perl ];
}
