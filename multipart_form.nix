{ buildDunePackage, pkgs, lib, stdenv, fetchurl, ocaml, dune_3 }:

buildDunePackage rec {
  pname = "multipart_form";
  version = "0.5.0";
  duneVersion = "3";

  src = fetchurl {
    url = "https://github.com/dinosaure/multipart_form/releases/download/v0.5.0/multipart_form-0.5.0.tbz";
    hash = "sha256-qKNsHA4oc7obO9MszfuPtnZuBmEuUuNrMHemopaoimQ=";
  };

  buildInputs = with pkgs.ocamlPackages; [ ke lwt multipart-form-data angstrom uutf pecu prettym base64 unstrctrd logs ];

}


