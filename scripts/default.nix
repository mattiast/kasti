{ nixPkgs ? import <nixpkgs> {}}:
with nixPkgs;
let
  python = (import ./python.nix { pkgs = nixPkgs; }).python;
  scriptImage = dockerTools.buildImage {
    name = "kasti-scripts";
    tag = "latest";
    contents = [
      ./src
      python
    ];
    config = {
      Entrypoint = [
        "${python}/bin/python"
      ];
      WorkingDir = "/";
    };
  };
in
  scriptImage
