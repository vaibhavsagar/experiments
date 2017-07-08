let
  pkgs = import <nixpkgs> {};
  src = pkgs.fetchFromGitHub {
    owner  = "gibiansky";
    repo   = "IHaskell";
    rev    = "a184f94f67d87c800890c8ca23e552aa51ee2ec4";
    sha256 = "0i3xkvj703da90bdhbfyyb2ihyny23mvkxay1jzxj2clj1a9d05r";
  };
  dontCheck = pkgs.haskell.lib.dontCheck;
  haskellPackages = pkgs.haskellPackages.override {
    overrides = self: super: {
      ihaskell       = dontCheck (
                          self.callCabal2nix "ihaskell"          src                                   { bin-package-db = null; });
      ghc-parser        = self.callCabal2nix "ghc-parser"     "${src}/ghc-parser"                      {};
      ipython-kernel    = self.callCabal2nix "ghc-parser"     "${src}/ipython-kernel"                  {};
      ihaskell-blaze    = self.callCabal2nix "ihaskell-blaze" "${src}/ihaskell-display/ihaskell-blaze" {};
      ihaskell-diagrams = self.callCabal2nix "ihaskell-blaze" "${src}/ihaskell-display/ihaskell-blaze" {};
    };
  };
  ihaskell = haskellPackages.ihaskell;
  ihaskellEnv = haskellPackages.ghcWithPackages (self: with self; [
    ihaskell
    ihaskell-blaze
    ihaskell-diagrams
  ]);
  jupyter = pkgs.python3.buildEnv.override {
    extraLibs = with pkgs.python3Packages; [ ipython ipykernel jupyter_client notebook ];
  };
  ihaskellSh = pkgs.writeScriptBin "ihaskell-notebook" ''
    #! ${pkgs.stdenv.shell}
    export GHC_PACKAGE_PATH="$(echo ${ihaskellEnv}/lib/*/package.conf.d| tr ' ' ':'):$GHC_PACKAGE_PATH"
    export PATH="${pkgs.stdenv.lib.makeBinPath [ ihaskell ihaskellEnv jupyter ]}"
    ${ihaskell}/bin/ihaskell install -l $(${ihaskellEnv}/bin/ghc --print-libdir) && ${jupyter}/bin/jupyter notebook
  '';
  profile = "${ihaskell.pname}-${ihaskell.version}/profile/profile.tar";
in
pkgs.buildEnv {
  name = "ihaskell-with-packages";
  paths = [ ihaskellEnv jupyter ];
  postBuild = ''
    . "${pkgs.makeWrapper}/nix-support/setup-hook"
    ln -s ${ihaskellSh}/bin/ihaskell-notebook $out/bin/.
    for prg in $out/bin"/"*;do
      wrapProgram $prg --set PYTHONPATH "$(echo ${jupyter}/lib/*/site-packages)"
    done
  '';
}
