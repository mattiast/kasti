{ mkDerivation, aeson, base, Diff, elm-export, fetchgit, hspec
, HUnit, lens, servant, servant-foreign, stdenv, text
, wl-pprint-text
}:
mkDerivation {
  pname = "servant-elm";
  version = "0.5.0.0";
  src = fetchgit {
    url = "https://github.com/mattiast/servant-elm.git";
    sha256 = "1ka3ppkyjc8y46vb2sn45r5v0wr71aqa7yk8bsj3gl2mx70z850p";
    rev = "42b14455d7a09c34389d7ba7aa5e80202802387a";
    fetchSubmodules = true;
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base elm-export lens servant servant-foreign text wl-pprint-text
  ];
  testHaskellDepends = [
    aeson base Diff elm-export hspec HUnit servant text
  ];
  doCheck = false;
  homepage = "http://github.com/mattjbray/servant-elm#readme";
  description = "Automatically derive Elm functions to query servant webservices";
  license = stdenv.lib.licenses.bsd3;
}
