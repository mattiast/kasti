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
    python-language-server
    pyls-isort
    pyls-black
    pyls-mypy
  ]
  )
