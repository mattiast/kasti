{ nixPkgs ? import <nixpkgs> {}}:
with nixPkgs;
let
  python = import ./python.nix { pkgs = nixPkgs; };
  myVim = import ./vim.nix { pkgs = nixPkgs; python = python; };
in
  stdenv.mkDerivation rec {
    name = "kasti-scripts";
    env = buildEnv { name = name; paths = buildInputs; };
    buildInputs = [
      python
      myVim
      git
    ];
  }
