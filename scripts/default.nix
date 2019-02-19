{ nixPkgs ? import <nixpkgs> {}}:
with nixPkgs;
let
  python = import ./python.nix { pkgs = nixPkgs; };
  scriptImage = dockerTools.buildImage {
    name = "kasti-scripts";
    tag = "latest";
    contents = [
      ./.
      python
    ];
  };
in
  scriptImage
