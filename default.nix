{ nixPkgs ? import <nixpkgs> {}}:
with nixPkgs;
let
  python = import ./scripts/python.nix { pkgs = nixPkgs; };
  myVim = import ./scripts/vim.nix { pkgs = nixPkgs; python = python; };
  neoVim = import ./neovim.nix { pkgs = nixPkgs; };
  jsFile = import ./frontend/default.nix { nixpkgs = nixPkgs; };
  scriptImage = import ./scripts/default.nix { nixPkgs = nixPkgs; };
  kasti-exe = (import ./backend/default.nix { nixPkgs = nixPkgs; }).kasti-minimal;
  image = nixPkgs.dockerTools.buildImage {
    name = "kasti-container";
    config.Cmd = [ "${kasti-exe}/bin/kasti-server" ];
  };

  env = stdenv.mkDerivation rec {
    name = "kasti-env";
    env = buildEnv { name = name; paths = buildInputs; };
    buildInputs = [
      python
      myVim
      neoVim
      git
      elmPackages.elm
      elmPackages.elm-format
      cabal-install
      haskellPackages.ghc
      haskellPackages.ghcid
    ];
  };

in {
  inherit env image scriptImage jsFile kasti-exe;
}
