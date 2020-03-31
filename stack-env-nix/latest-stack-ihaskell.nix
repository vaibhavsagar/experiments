let
  nixpkgs-src = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/tarball/6d445f8398d2d585d20d9acacf00fd9d15081b3b";
    sha256 = "1ajd0zr31iny3g9hp0pc1y2pxcm3nakdv6260lnvyn0k4vygync2";
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
