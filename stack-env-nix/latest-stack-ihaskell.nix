let
  nixpkgs-src = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/tarball/4f31c024d1e40b8e19badc832c8a99ce19143a5b";
    sha256 = "0gqkf7spy60y4bycy3f3fg99wv77q7mhkg8c1a4s3gw51nakwvq8";
  };
  pkgs = import nixpkgs-src {};
in pkgs.buildFHSUserEnv {
  name = "fhs";
  targetPkgs = pkgs: [
    pkgs.binutils
    pkgs.binutils.bintools
    pkgs.blas
    pkgs.cairo.dev
    pkgs.curl
    pkgs.file
    pkgs.gcc
    pkgs.glib.dev
    pkgs.gmp
    pkgs.gnumake
    pkgs.haskell.compiler.ghc865
    pkgs.cabal-install
    pkgs.haskellPackages.alex
    pkgs.haskellPackages.cpphs
    pkgs.haskellPackages.happy
    pkgs.haskellPackages.stack
    pkgs.iana-etc
    pkgs.liblapack
    pkgs.pango.dev
    pkgs.perl
    pkgs.pkgconfig
    (pkgs.python3.withPackages (ps: [ ps.jupyter ps.notebook ]))
    pkgs.ncurses
    pkgs.zeromq
    pkgs.zlib.dev
  ];
  profile = ''
    export C_INCLUDE_PATH=/usr/include:$C_INCLUDE_PATH
  '';
}
