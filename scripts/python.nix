{ pkgs }:
let
  python = pkgs.python37.withPackages (
    pyPkgs: with pyPkgs; [
      lxml
      PyRSS2Gen
      boto3
      psycopg2
      requests
    ]);
  python-dev = python.withPackages (
    pyPkgs: with pyPkgs; [
      jedi
      mypy
      python-language-server
      pyls-isort
      pyls-black
      pyls-mypy
    ]);
in
  { inherit python python-dev; }
