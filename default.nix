{ pkgs ? import <nixpkgs> { } }:
let
  buildDunePackage = pkgs.ocamlPackages.buildDunePackage;
  dream-pure = pkgs.callPackage ./dream-pure.nix { inherit buildDunePackage; };
  multipart_form = pkgs.callPackage ./multipart_form.nix { inherit buildDunePackage; };
  dream-httpaf = pkgs.callPackage ./dream-httpaf.nix { inherit multipart_form; inherit dream-pure; inherit buildDunePackage; };
  clap = pkgs.callPackage ./clap.nix { inherit buildDunePackage; };
  multipart_form-lwt = pkgs.callPackage ./multipart_form-lwt.nix { inherit multipart_form; inherit buildDunePackage; };
  dream = pkgs.callPackage ./dream.nix {
    inherit dream-pure; inherit dream-httpaf; inherit multipart_form; inherit multipart_form-lwt; inherit buildDunePackage;
  };
in
pkgs.mkShell {
  packages = with pkgs.ocamlPackages; [
    ocaml
    base
    reason
    stdio
    alcotest
    alcotest-lwt
    lwt
    odoc
    pkgs.opam
    pkgs.openssl
    utop
    dune_3
    dream
    dream-pure
    dream-httpaf
    clap
    ptime
    mirage-crypto-rng-lwt
    graphql-lwt
    caqti-lwt
    hmap
    base64
    unstrctrd
    mirage-clock
    magic-mime
    digestif
    pecu
    prettym
    lwt_ssl
    faraday-lwt-unix
    psq
    multipart_form-lwt
    multipart_form
  ];
}
