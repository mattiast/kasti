{ mkDerivation, base, bytestring, containers, Diff, directory
, fetchgit, formatting, hspec, hspec-core, HUnit, mtl, QuickCheck
, quickcheck-instances, stdenv, text, time, wl-pprint-text
}:
mkDerivation {
  pname = "elm-export";
  version = "0.6.0.1";
  src = fetchgit {
    url = "https://github.com/mattiast/elm-export";
    sha256 = "0rr48anlbf6chhnhfzzvk0a1fh25anjzd43jchs5ha3ikpqgnbz5";
    rev = "cca124bda9f1d2226e3134e41f05ebb8c3bb7eaa";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    base bytestring containers directory formatting mtl text time
    wl-pprint-text
  ];
  testHaskellDepends = [
    base bytestring containers Diff hspec hspec-core HUnit QuickCheck
    quickcheck-instances text time
  ];
  doCheck = false;
  homepage = "http://github.com/krisajenkins/elm-export";
  description = "A library to generate Elm types from Haskell source";
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
