{ nixPkgs ? import <nixpkgs> {}}:
with nixPkgs;
let
  python = (import ./scripts/python.nix { pkgs = nixPkgs; }).python-dev;
  neoVim = import ./neovim.nix { pkgs = nixPkgs; };
  scriptImage = import ./scripts/default.nix { nixPkgs = nixPkgs; };
  backend = (import ./backend/default.nix { nixPkgs = nixPkgs; });
  kasti-exe = backend.kasti-minimal;
  kasti-elm-client = stdenv.mkDerivation {
    name = "kasti-elm-client";
    unpackPhase = "true"; # no sources needed

    buildInputs = [ kasti-exe ];

    installPhase = ''
      mkdir -p $out/Client
      ${kasti-exe}/bin/elm-gen $out
    '';
  };

  jsFile = import ./frontend/default.nix { nixpkgs = nixPkgs; kasti-elm-client = kasti-elm-client; };
  htmlFile = ./frontend/browse.html;

  image = nixPkgs.dockerTools.buildImage {
    name = "kasti-backend";
    tag = "latest";
    config.Cmd = [ "${kasti-exe}/bin/kasti-server" ];
    config.Env = [
      "HTML_PATH=${htmlFile}"
      "JS_PATH=${jsFile}"
    ];
    contents = [ iana-etc cacert ];
  };

  env = stdenv.mkDerivation rec {
    name = "kasti-env";
    env = buildEnv { name = name; paths = buildInputs; };
    buildInputs = [
      python
      neoVim
      git
      elmPackages.elm
      elmPackages.elm-format
      cabal-install
      backend.kasti.compiler
      haskellPackages.ghcid
      fzf
    ];
  };

in {
  inherit env image scriptImage jsFile kasti-exe kasti-elm-client;
}
