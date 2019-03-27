{ stdenv, fetchurl, bison }:

stdenv.mkDerivation {

  name = "hugs98-200609";

  src = ./.;

  nativeBuildInputs = [ bison ];

  postUnpack = "find -type f -exec sed -i 's@/bin/cp@cp@' {} +";

  preConfigure = "unset STRIP";

  configureFlags = [
    "--enable-char-encoding=utf8"       # require that the UTF-8 encoding is always used
    "--disable-path-canonicalization"
    "--disable-timer"                   # evaluation timing (for benchmarking Hugs)
    "--disable-profiling"               # heap profiler
    "--disable-stack-dumps"             # stack dump on stack overflow
    "--enable-large-banner"             # multiline startup banner
    "--disable-internal-prims"          # experimental primitives to access Hugs's innards
    "--disable-debug"                   # include C debugging information (for debugging Hugs)
    "--disable-tag"                     # runtime tag checking (for debugging Hugs)
    "--disable-lint"                    # "lint" flags (for debugging Hugs)
    "--disable-only98"                  # build Hugs to understand Haskell 98 only
    "--enable-ffi"
    "--enable-pthreads"                 # build Hugs using POSIX threads C library
  ];

  meta = with stdenv.lib; {
    homepage = https://www.haskell.org/hugs;
    description = "Haskell interpreter";
    maintainers = with maintainers; [ joachifm ];
    license = licenses.bsd3;
    platforms = platforms.all;
  };
}
