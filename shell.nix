let
  sources = import ./nix/sources.nix {};
  haskellNix = import sources.haskellNix {};
  pkgs = import haskellNix.sources.nixpkgs-unstable haskellNix.nixpkgsArgs;
  python-custom = (pkgs.python3.withPackages (
        pythonPackages: with pythonPackages; [
          chess
        ]));
in
(import ./default.nix).shellFor {
  buildInputs = [
    python-custom
    pkgs.pcre
    pkgs.stockfish
  ];
  tools = {
    cabal = "latest";
    hlint = "latest";
    haskell-language-server = "latest";
  };
  PY_BASE_RAW = "${pkgs.python3}";
  PY_BASE = "${python-custom}";
  PY_PRE = "${python-custom.libPrefix}";
  STOCKFISH_PATH="${pkgs.stockfish}/bin/stockfish";
}
