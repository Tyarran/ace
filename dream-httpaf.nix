{ buildDunePackage, stdenv, pkgs, lib, ocaml, fetchurl, dream-pure, multipart_form }:
buildDunePackage rec {
  pname = "dream-httpaf";
  version = "1.0.0-alpha5";
  duneVersion = "3";

  src = fetchurl {
    url = "https://github.com/aantron/dream/releases/download/${version}/dream-${version}.tar.gz";
    hash = "sha256-Bbx/av+UiTwVHzpbze4zKJIMabN2P6xJuic0jcd0WQE=";
  };

  buildInputs = with pkgs.ocamlPackages; [ digestif lwt base64 faraday faraday-lwt-unix angstrom lwt_ppx result ke psq dream-pure ptime uri hmap gluten-lwt-unix ssl multipart_form ];


}

