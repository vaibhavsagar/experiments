let
  inherit (import <nixpkgs> {}) bash fetchFromGitHub runCommand;
  CRoaring = fetchFromGitHub {
    owner = "RoaringBitmap";
    repo = "CRoaring";
    rev = "083d39855d6ba51d3645929fcd44153a1b2f14d8";
    sha256 = "1pvbhfgdnl5cdc4iy6g6yfsbh7ybcsd1x0dxp2pcimcrlz9581a0";
  };
in runCommand "croaring" {
  buildInputs = [bash];
} ''
  mkdir -p $out
  TMP=$(mktemp -d)
  cp -R ${CRoaring} $TMP
  cd $TMP/*-source
  chmod -R u+w .
  bash amalgamation.sh
  cp roaring.c roaring.h $out
''
