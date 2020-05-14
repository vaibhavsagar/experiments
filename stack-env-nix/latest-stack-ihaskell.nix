let
  nixpkgs-src = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/tarball/4b6bfecc0bd30c7740b9e9fa849de9e5df8d6e26";
    sha256 = "0wi8641v4345jcqk7myq3dknczps1gbsf48q314wavl5kga8r67f";
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
    pkgs.haskell.compiler.ghc883
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
