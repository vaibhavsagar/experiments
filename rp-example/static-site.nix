let
  # ./updater versions.json reflex-platform
  fetcher = { owner, repo, rev, sha256, ... }: builtins.fetchTarball {
    inherit sha256;
    url = "https://github.com/${owner}/${repo}/tarball/${rev}";
  };
  reflex-platform = fetcher (builtins.fromJSON (builtins.readFile ./versions.json)).reflex-platform;
  pkgs = (import reflex-platform {}).nixpkgs;
  project = import ./default.nix;
  html = pkgs.writeTextFile {
    name = "index.html";
    text = ''
      <!DOCTYPE html>
      <html>
        <head>
          <script defer src="https://cdn.jsdelivr.net/npm/viz.js@2.1.2/viz.min.js"></script>
          <script defer src="https://cdn.jsdelivr.net/npm/viz.js@2.1.2/full.render.min.js"></script>
          <script defer language="javascript" src="all.js"></script>
        </head>
        <body>
        </body>
      </html>
    '';
  };
  minifiedJs = pkgs.runCommand "all.js" {} ''
    ${pkgs.closurecompiler}/bin/closure-compiler \
      --externs=${project.ghcjs8_10.hamt}/bin/hamt.jsexe/all.js.externs \
      --jscomp_off=checkVars \
      --js_output_file="$out" \
      -O ADVANCED \
      -W QUIET \
      ${project.ghcjs8_10.hamt}/bin/hamt.jsexe/all.js
  '';
in pkgs.runCommand "static-site" {} ''
  mkdir -p $out
  cp ${html} $out/index.html
  cp ${minifiedJs} $out/all.js
''
