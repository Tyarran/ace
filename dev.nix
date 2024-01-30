{ pkgs ? import <nixpkgs> { } }:

pkgs.mkShell {
  packages = with pkgs; [
    ocaml
    opam
    git
    openssl
    libev
    sqlite
    python3
    pkg-config
    gnumake
    curl
    cacert
  ];

  shellHook = ''
    export OPAMNODEPEXTS=1
    eval $(opam env)
  '';
}
