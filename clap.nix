{ buildDunePackage, stdenv, pkgs, dune_3, lib, ocaml, fetchurl }:
buildDunePackage rec {
  pname = "clap";
  version = "0.3.0";
  duneVersion = "3";

  src = fetchurl {
    url = "https://github.com/rbardou/clap/archive/0.3.0.tar.gz";
    hash = "sha256-vWEinllWqVYg5Yzru9agEnx0GTLoffSwp/1W7ZIq5cE=";
  };

  buildInputs = with pkgs.ocamlPackages; [ ];
}

