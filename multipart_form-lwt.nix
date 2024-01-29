{ buildDunePackage, pkgs, lib, stdenv, fetchurl, ocaml, dune_3, multipart_form }:

buildDunePackage rec {
  pname = "multipart_form-lwt";
  version = "0.5.0";
  duneVersion = "3";

  src = fetchurl {
    url = "https://github.com/dinosaure/multipart_form/releases/download/v${version}/multipart_form-${version}.tbz";
    hash = "sha256-qKNsHA4oc7obO9MszfuPtnZuBmEuUuNrMHemopaoimQ=";
  };

  buildInputs = with pkgs.ocamlPackages; [ ke lwt multipart_form angstrom uutf pecu prettym base64 unstrctrd logs ];

}

