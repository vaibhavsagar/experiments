let
  inherit (import <nixpkgs> {}) bash fetchFromGitHub runCommand;
  CRoaring = fetchFromGitHub {
    owner = "RoaringBitmap";
    repo = "CRoaring";
    rev = "0a5c300a7a0391e5ec75465332ac7728e18b677f";
    sha256 = "1bl99n3yavgja8051b81w9lhmd0ahzy4l7s559p3jz966c1zypqf";
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
