{ nixpkgs ? import <nixpkgs> {}
, kasti-elm-client
}:

with nixpkgs;

let
  srcs = ./elm-srcs.nix;
  versionsDat = ./versions.dat;
in stdenv.mkDerivation {
  name = "kasti-frontend.js";
  src = ./.;

  buildInputs = [ elmPackages.elm kasti-elm-client ];

  buildPhase = elmPackages.fetchElmDeps {
    elmPackages = import srcs;
    inherit versionsDat;
  };

  installPhase = ''
    cp -R ${kasti-elm-client}/* src
    ${elmPackages.elm}/bin/elm make src/Browse.elm --output $out
  '';
}
