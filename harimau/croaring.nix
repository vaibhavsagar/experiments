let
  inherit (import <nixpkgs> {}) bash fetchFromGitHub runCommand;
  CRoaring = fetchFromGitHub {
    owner = "RoaringBitmap";
    repo = "CRoaring";
    rev = "24835ad92064137eda061abfc019dd2b4ebc87a9";
    sha256 = "ACFcbg+IdpRIQlqsqb1wtIT+N7zOW9fR+faDajSUM8c=";
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
