{ nixPkgs ? import <nixpkgs> {}}:
with nixPkgs;
let
  python = import ./scripts/python.nix { pkgs = nixPkgs; };
  myVim = import ./scripts/vim.nix { pkgs = nixPkgs; python = python; };
  jsFile = import ./frontend/default.nix { nixpkgs = nixPkgs; };
  scriptImage = import ./scripts/default.nix { nixPkgs = nixPkgs; };
in
  stdenv.mkDerivation rec {
    name = "kasti-env";
    env = buildEnv { name = name; paths = buildInputs; };
    buildInputs = [
      python
      myVim
      git
      elmPackages.elm
      elmPackages.elm-format
    ];
  }
