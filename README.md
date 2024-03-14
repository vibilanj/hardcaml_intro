### Initialize the project
```sh
dune init proj hardcaml_intro
opam switch create .

opam repo add janestreet-bleeding https://github.com/janestreet/opam-repository.git
opam install hardcaml hardcaml_waveterm ppx_hardcaml merlin ocamlformat ocaml-lsp-server utop 

opam switch export hardcaml-intro-env
```

### To reinstall
```sh
opam switch import hardcaml-intro-env --switch hardcaml_intro
opam switch reinstall hardcaml_intro
eval $(opam env)
```

refer to [opam-switch manpage](https://opam.ocaml.org/doc/man/opam-switch.html)

## Build and run

```sh
dune build
# dune exec hardcaml_intro
dune exec -- hardcaml_intro -s
```

### Autoformat

```sh
dune fmt # whole project
ocamlformat # single file
```
