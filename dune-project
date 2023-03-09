(lang dune 3.6)
(name ace)
(version 0.1.0)
(generate_opam_files true)

(authors "Romain Commandé")

(package
 (name ace)
 (synopsis "Ace : Another chatbot engine")
 (description "Ace : Another chatbot engine")
 ; (depends ANSITerminal base (cmdliner (= 1.0.4)) cohttp-lwt-unix dream lambda-term lwt re2 reason tls yaml yojson ocaml alcotest))
 (depends base stdio reason ocaml (alcotest :with-test) (alcotest-lwt :with-test) conf-libev lwt))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
