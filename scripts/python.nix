{ pkgs }:
let
  run-pkgs = ps: with ps; [
    lxml
    PyRSS2Gen
    boto3
    psycopg2
    requests
  ];
  dev-pkgs = ps: with ps; [
    jedi
    mypy
    python-language-server
    pyls-isort
    pyls-black
    pyls-mypy
  ];
  python = pkgs.python37.withPackages run-pkgs;
  python-dev = python.withPackages (
    ps: run-pkgs ps ++ dev-pkgs ps
  );
in
  { inherit python python-dev; }
