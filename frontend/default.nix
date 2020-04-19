{ nixpkgs ? import <nixpkgs> {}
, kasti-elm-client
}:

with nixpkgs;

let
  srcs = ./elm-srcs.nix;
  registryDat = ./registry.dat;
in stdenv.mkDerivation {
  name = "kasti-frontend.js";
  src = ./.;

  buildInputs = [ elmPackages.elm kasti-elm-client ];

  buildPhase = elmPackages.fetchElmDeps {
    elmPackages = import srcs;
    elmVersion = "0.19.1";
    inherit registryDat;
  };

  installPhase = ''
    cp -R ${kasti-elm-client}/* src
    elm make src/Main.elm --output $out
  '';
}
