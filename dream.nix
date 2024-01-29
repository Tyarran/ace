{ buildDunePackage, pkgs, lib, stdenv, fetchurl, ocaml, dream-pure, dream-httpaf, multipart_form, multipart_form-lwt }:

pkgs.ocamlPackages.buildDunePackage rec {
  pname = "dream";
  version = "1.0.0-alpha5";
  duneVersion = "3";

  src = fetchurl {
    url = "https://github.com/aantron/dream/releases/download/${version}/dream-${version}.tar.gz";
    hash = "sha256-Bbx/av+UiTwVHzpbze4zKJIMabN2P6xJuic0jcd0WQE=";
  };

  buildInputs = with pkgs.ocamlPackages; [ lwt_ppx yojson magic-mime uri unstrctrd ptime camlp-streams mirage-crypto-rng-lwt graphql-lwt dream-pure dream-httpaf digestif multipart-form-data caqti-lwt hmap multipart_form-lwt lwt_ssl base64 multipart_form mirage-clock pecu prettym faraday-lwt-unix psq ];


  meta = with lib; {
    homepage = "https://github.com/mirage/alcotest";
    description = "A lightweight and colourful test framework";
    license = licenses.isc;
    maintainers = [ maintainers.ericbmerritt ];
  };
}

