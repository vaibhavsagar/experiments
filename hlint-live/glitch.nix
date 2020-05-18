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
          <script language="javascript" src="all.js"></script>
        </head>
        <body>
        </body>
      </html>
    '';
  };
in pkgs.runCommand "glitch" {} ''
  mkdir -p $out
  cp ${html} $out/index.html
  ${pkgs.closurecompiler}/bin/closure-compiler \
    --externs=${project.ghcjs.hlint-live}/bin/hlint-live.jsexe/all.js.externs \
    --jscomp_off=checkVars \
    --js_output_file="$out/all.js" \
    -O ADVANCED \
    -W QUIET \
    ${project.ghcjs.hlint-live}/bin/hlint-live.jsexe/all.js
''
