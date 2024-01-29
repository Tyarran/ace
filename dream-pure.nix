{ buildDunePackage, stdenv, lib, fetchurl, ocaml, pkgs, ... }:
buildDunePackage rec {
  pname = "dream-pure";
  version = "1.0.0-alpha5";

  src = fetchurl {
    url = "https://github.com/aantron/dream/releases/download/${version}/dream-${version}.tar.gz";
    hash = "sha256-Bbx/av+UiTwVHzpbze4zKJIMabN2P6xJuic0jcd0WQE=";
  };

  buildInputs = with pkgs.ocamlPackages; [ ptime lwt_ppx uri hmap base64 multipart-form-data digestif caqti-lwt ];
}
