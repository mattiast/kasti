{ pkgs }:
pkgs.python37.withPackages (
  pyPkgs: with pyPkgs; [
    lxml
    PyRSS2Gen
    boto3
    psycopg2
    requests
    jedi
    mypy
  ]
  )
