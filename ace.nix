{ pkgs, lib, stdenv, fetchurl, ocaml, dune_3 }:

let
  Dune = dune_3;
  dream = pkgs.callPackage ./dream.nix { };
  # dream-pure = pkgs.callPackage ./dream-pure.nix { };
  # dream-httpaf = pkgs.callPackage ./dream-httpaf.nix { };
  # multipart_form-lwt = pkgs.callPackage ./multipart_form-lwt.nix { };
  # multipart_form = pkgs.callPackage ./multipart_form.nix { };
in
stdenv.mkDerivation rec {
  pname = "ace";
  version = "0.1.0";

  duneVersion = "3";

  minimalOCamlVersion = "4.08";

  buildPhase = ''
    runHook preBuild
    dune build -p ${pname} ''${enableParallelBuilding:+-j $NIX_BUILD_CORES}
    runHook postBuild
  '';
  checkPhase = ''
    runHook preCheck
    dune runtest -p ${pname} ''${enableParallelBuilding:+-j $NIX_BUILD_CORES}
    runHook postCheck
  '';
  installPhase = ''
    runHook preInstall
    dune install --prefix $out --libdir $OCAMLFIND_DESTDIR ${pname} \
     ${if lib.versionAtLeast Dune.version "2.9"
       then "--docdir $out/share/doc --mandir $out/share/man"
       else ""}
    runHook postInstall
  '';

  src = ./.;
  # src = fetchurl {
  #   url = "https://github.com/aantron/dream/releases/download/${version}/dream-${version}.tar.gz";
  #   hash = "sha256-Bbx/av+UiTwVHzpbze4zKJIMabN2P6xJuic0jcd0WQE=";
  # };

  nativeBuildInputs = [ ocaml Dune pkgs.ocamlPackages.findlib ];
  buildInputs = with pkgs.ocamlPackages; [ lwt_ppx yojson magic-mime uri unstrctrd ptime camlp-streams mirage-crypto-rng-lwt graphql-lwt dream-pure dream-httpaf digestif multipart-form-data caqti-lwt hmap multipart_form-lwt lwt_ssl base64 multipart_form mirage-clock pecu prettym faraday-lwt-unix psq ];


  # meta = with lib; {
  #   homepage = "https://github.com/mirage/alcotest";
  #   description = "A lightweight and colourful test framework";
  #   license = licenses.isc;
  #   maintainers = [ maintainers.ericbmerritt ];
  # };
}

