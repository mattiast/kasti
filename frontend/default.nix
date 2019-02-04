with import <nixpkgs> {};
stdenv.mkDerivation rec {
  name = "kasti-frontend";
  env = buildEnv { name = name; paths = buildInputs; };
  buildInputs = [
    elmPackages.elm
  ];
}
