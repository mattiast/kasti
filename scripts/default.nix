{ nixPkgs ? import <nixpkgs> {}}:
with nixPkgs;
let
  python = (import ./python.nix { pkgs = nixPkgs; }).python;
  pythonBaseImage = dockerTools.buildImage {
    name = "python-base";
    contents = [
      python
    ];
  };
  scriptImage = dockerTools.buildImage {
    name = "kasti-scripts";
    tag = "latest";
    fromImage = pythonBaseImage;
    contents = [
      ./src
    ];
  };
in
  scriptImage
