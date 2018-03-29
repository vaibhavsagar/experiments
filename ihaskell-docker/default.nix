{ packages ? (_: []), rtsopts ? "-M3g -N2", systemPackages ? (_: []) }:

let
  inherit (import <nixpkgs> {}) fetchFromGitHub;
  nixpkgs = import (fetchFromGitHub {
    owner  = "NixOS";
    repo   = "nixpkgs";
    rev    = "d16ffa1509008b53891412b095cea259dd73d869";
    sha256 = "0ir4cbld2c5h31aiap9jhj7mpl0nw2yvr9lmw6zn8ndidniv8ki3";
  }) {};
  inherit (builtins) any elem filterSource listToAttrs;
  src = nixpkgs.fetchFromGitHub {
    owner = "gibiansky";
    repo = "IHaskell";
    rev = "2d43d305d1b0e50ed8dcdcc6b959ef3f3d9cdc0e";
    sha256 = "0y1a13irikyziva5b4fh3vhmncrh0fdn4dj7k2ar65zj41yd9l13";
  };
  lib = nixpkgs.lib;
  cleanSource = name: type: let
    baseName = baseNameOf (toString name);
  in lib.cleanSourceFilter name type && !(
    (type == "directory" && (elem baseName [ ".stack-work" "dist"])) ||
    any (lib.flip lib.hasSuffix baseName) [ ".hi" ".ipynb" ".nix" ".sock" ".yaml" ".yml" ]
  );
  ihaskell-src         = src;
  ipython-kernel-src   = "${src}/ipython-kernel";
  ghc-parser-src       = "${src}/ghc-parser";
  ihaskell-display-src = "${src}/ihaskell-display";
  displays = self: listToAttrs (
    map
      (display: { name = display; value = self.callCabal2nix display "${ihaskell-display-src}/${display}" {}; })
      [
        "ihaskell-aeson"
        "ihaskell-blaze"
        "ihaskell-charts"
        "ihaskell-diagrams"
        "ihaskell-gnuplot"
        "ihaskell-hatex"
        "ihaskell-juicypixels"
        "ihaskell-magic"
        "ihaskell-plot"
        "ihaskell-rlangqq"
        "ihaskell-static-canvas"
        "ihaskell-widgets"
      ]);
  haskellPackages = nixpkgs.haskellPackages.override {
    overrides = self: super: {
      ihaskell       = nixpkgs.haskell.lib.overrideCabal (
                       self.callCabal2nix "ihaskell"       ihaskell-src       {}) (_drv: {
        preCheck = ''
          export HOME=$(${nixpkgs.pkgs.coreutils}/bin/mktemp -d)
          export PATH=$PWD/dist/build/ihaskell:$PATH
          export GHC_PACKAGE_PATH=$PWD/dist/package.conf.inplace/:$GHC_PACKAGE_PATH
        '';
      });
      ghc-parser     = self.callCabal2nix "ghc-parser"     ghc-parser-src     {};
      ipython-kernel = self.callCabal2nix "ipython-kernel" ipython-kernel-src {};

      haskell-src-exts  = self.haskell-src-exts_1_20_1;
      haskell-src-meta  = self.haskell-src-meta_0_8_0_2;
      hmatrix           = self.hmatrix_0_18_2_0;

      shelly            = nixpkgs.haskell.lib.doJailbreak super.shelly;
      static-canvas     = nixpkgs.haskell.lib.doJailbreak super.static-canvas;
    } // displays self;
  };
  ihaskellEnv = haskellPackages.ghcWithPackages (self: [ self.ihaskell ] ++ packages self);
  jupyter = nixpkgs.python3.withPackages (ps: [ ps.jupyter ps.notebook ]);
  ihaskellSh = nixpkgs.writeScriptBin "ihaskell-notebook" ''
    #! ${nixpkgs.stdenv.shell}
    export GHC_PACKAGE_PATH="$(echo ${ihaskellEnv}/lib/*/package.conf.d| ${nixpkgs.coreutils}/bin/tr ' ' ':'):$GHC_PACKAGE_PATH"
    export PATH="${nixpkgs.stdenv.lib.makeBinPath ([ ihaskellEnv jupyter ] ++ systemPackages nixpkgs)}"
    ${ihaskellEnv}/bin/ihaskell install -l $(${ihaskellEnv}/bin/ghc --print-libdir) --use-rtsopts="${rtsopts}" && \
    ${jupyter}/bin/jupyter notebook --allow-root --NotebookApp.port=8888 '--NotebookApp.ip=*' --NotebookApp.notebook_dir=/notebooks
  '';
  fullEnvironment = nixpkgs.buildEnv {
    name = "ihaskell-with-packages";
    buildInputs = [ nixpkgs.makeWrapper ];
    paths = [ ihaskellEnv jupyter ];
    postBuild = ''
      ${nixpkgs.coreutils}/bin/ln -s ${ihaskellSh}/bin/ihaskell-notebook $out/bin/
      for prg in $out/bin"/"*;do
        if [[ -f $prg && -x $prg ]]; then
          wrapProgram $prg --set PYTHONPATH "$(echo ${jupyter}/lib/*/site-packages)"
        fi
      done
    '';
  };
  dockerImage = nixpkgs.dockerTools.buildImage {
    name = "ihaskell";
    contents = fullEnvironment;
    runAsRoot = ''
      mkdir -p /notebooks
      mkdir -p /tmp
    '';
    config = {
      Cmd = [ "/bin/ihaskell-notebook" ];
      Env = [
        ''GHC_PACKAGE_PATH="$(echo ${ihaskellEnv}/lib/*/package.conf.d| ${nixpkgs.coreutils}/bin/tr ' ' ':'):$GHC_PACKAGE_PATH"''
        ''PATH="${nixpkgs.stdenv.lib.makeBinPath ([ ihaskellEnv jupyter ] ++ systemPackages nixpkgs)}"''
      ];
      ExposedPorts = {
        "8888/tcp" = {};
      };
      WorkingDir = "/notebooks";
      Volumes = {
        "/notebooks" = {};
        "/tmp" = {};
      };
    };
  };
  in dockerImage
